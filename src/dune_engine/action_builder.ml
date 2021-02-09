open! Stdune
open Import

type label = ..

type 'a t =
  | Pure : 'a -> 'a t
  | Map : ('a -> 'b) * 'a t -> 'b t
  | Both : 'a t * 'b t -> ('a * 'b) t
  | Seq : unit t * 'b t -> 'b t
  | All : 'a t list -> 'a list t
  | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t
  | Paths_for_rule : Path.Set.t -> unit t
  | Paths_glob : File_selector.t -> Path.Set.t t
  | If_file_exists : Path.t * 'a t * 'a t -> 'a t
  | Filter_existing_files : ('a * Path.Set.t) t -> ('a * Path.Set.t) t
  (* [Filter_existing_files] can't be defined using [If_file_exists] because in
     the latter the path must be known when building the type t. In the former
     case the paths can be dynamically computed *)
  | Contents : Path.t -> string t
  | Lines_of : Path.t -> string list t
  | Dyn_paths : ('a * Path.Set.t) t -> 'a t
  | Dyn_deps : ('a * Dep.Set.t) t -> 'a t
  | Label : label -> unit t
  | Or_exn : 'a Or_exn.t t -> 'a t
  | Fail : fail -> _ t
  | Memo : 'a memo -> 'a t
  | Catch : 'a t * 'a -> 'a t
  | Deps : Dep.Set.t -> unit t
  | Memo_build : 'a Memo.Build.t -> 'a t
  | Dyn_memo_build : 'a Memo.Build.t t -> 'a t
  | Build : 'a t t -> 'a t

and 'a memo =
  { name : string
  ; id : 'a Type_eq.Id.t
  ; t : 'a t
  }

(* We use forward declarations to pass the top-level [file_exists] function to
   avoid cyclic dependencies between modules. *)
let file_exists_fdecl = Fdecl.create Dyn.Encoder.opaque

let set_file_system_accessors ~file_exists =
  Fdecl.set file_exists_fdecl file_exists

let return x = Pure x

let ignore x = Map (Fun.const (), x)

let map x ~f = Map (f, x)

let map2 x y ~f = Map2 (f, x, y)

let delayed f = Map (f, Pure ())

let or_exn s = Or_exn s

module O = struct
  let ( >>> ) a b = Seq (a, b)

  let ( and+ ) a b = Both (a, b)

  let ( let+ ) t f = Map (f, t)
end

open O

let both x y = Both (x, y)

let all xs = All xs

let all_unit xs =
  let+ (_ : unit list) = all xs in
  ()

let label map = Label map

let deps d = Deps d

let dep d = Deps (Dep.Set.singleton d)

let dyn_deps x = Dyn_deps x

let path p = Deps (Dep.Set.singleton (Dep.file p))

let paths ps = Deps (Dep.Set.of_files ps)

let path_set ps = Deps (Dep.Set.of_files_set ps)

let paths_matching ~loc:_ dir_glob = Paths_glob dir_glob

let dyn_paths paths =
  Dyn_paths
    (let+ x, paths = paths in
     (x, Path.Set.of_list paths))

let dyn_paths_unit paths =
  Dyn_paths
    (let+ paths = paths in
     ((), Path.Set.of_list paths))

let dyn_path_set paths = Dyn_paths paths

let dyn_path_set_reuse paths =
  Dyn_paths
    (let+ paths = paths in
     (paths, paths))

let paths_for_rule ps = Paths_for_rule ps

let env_var s = Deps (Dep.Set.singleton (Dep.env s))

let alias a = dep (Dep.alias a)

let catch t ~on_error = Catch (t, on_error)

let contents p = Contents p

let lines_of p = Lines_of p

let strings p =
  let f x =
    match Scanf.unescaped x with
    | Error () ->
      User_error.raise
        [ Pp.textf "Unable to parse %s" (Path.to_string_maybe_quoted p)
        ; Pp.textf
            "This file must be a list of lines escaped using OCaml's \
             conventions"
        ]
    | Ok s -> s
  in
  Map ((fun l -> List.map l ~f), lines_of p)

let read_sexp p =
  let+ s = contents p in
  Dune_lang.Parser.parse_string s ~lexer:Dune_lang.Lexer.token
    ~fname:(Path.to_string p) ~mode:Single

let if_file_exists p ~then_ ~else_ = If_file_exists (p, then_, else_)

let file_exists p = if_file_exists p ~then_:(return true) ~else_:(return false)

let filter_existing_files p = Filter_existing_files p

let paths_existing paths =
  all_unit
    (List.map paths ~f:(fun file ->
         if_file_exists file ~then_:(path file) ~else_:(return ())))

let fail x = Fail x

let of_result = function
  | Ok x -> x
  | Error e -> fail { fail = (fun () -> raise e) }

let of_result_map res ~f =
  match res with
  | Ok x -> f x
  | Error e -> fail { fail = (fun () -> raise e) }

let memoize name t = Memo { name; id = Type_eq.Id.create (); t }

let source_tree ~dir =
  let dep_set = Dep.Set.source_tree dir in
  let+ () = deps dep_set in
  Dep.Set.paths dep_set

(* CR-someday amokhov: The set of targets is accumulated using information from
   multiple sources by calling [Path.Build.Set.union] and hence occasionally
   duplicate declarations of the very same target go unnoticed. I think such
   redeclarations are not erroneous but are merely redundant; it seems that it
   would be better to rule them out completely.

   Another improvement is to cache [Path.Build.Set.to_list targets] which is
   currently performed multiple times on the very same
   [Action_builder.With_targets.t]. *)
module With_targets = struct
  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

  let map_build t ~f = { t with build = f t.build }

  let return x = { build = Pure x; targets = Path.Build.Set.empty }

  let add t ~targets =
    { build = t.build
    ; targets = Path.Build.Set.union t.targets (Path.Build.Set.of_list targets)
    }

  let map { build; targets } ~f = { build = map build ~f; targets }

  let map2 x y ~f =
    { build = Map2 (f, x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let both x y =
    { build = Both (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  let seq x y =
    { build = Seq (x.build, y.build)
    ; targets = Path.Build.Set.union x.targets y.targets
    }

  module O = struct
    let ( >>> ) = seq

    let ( and+ ) = both

    let ( let+ ) a f = map ~f a
  end

  open O

  let all xs =
    match xs with
    | [] -> return []
    | xs ->
      let build, targets =
        List.fold_left xs ~init:([], Path.Build.Set.empty)
          ~f:(fun (xs, set) x ->
            (x.build :: xs, Path.Build.Set.union set x.targets))
      in
      { build = All (List.rev build); targets }

  let write_file_dyn fn s =
    add ~targets:[ fn ]
      (let+ s = s in
       Action.Write_file (fn, s))

  let of_result_map res ~f ~targets =
    add ~targets
      (match res with
      | Ok x -> f x
      | Error e ->
        { build = Fail { fail = (fun () -> raise e) }
        ; targets = Path.Build.Set.empty
        })

  let memoize name t = { build = memoize name t.build; targets = t.targets }
end

let with_targets build ~targets : _ With_targets.t =
  { build; targets = Path.Build.Set.of_list targets }

let with_targets_set build ~targets : _ With_targets.t = { build; targets }

let with_no_targets build : _ With_targets.t =
  { build; targets = Path.Build.Set.empty }

let write_file fn s =
  with_targets ~targets:[ fn ] (return (Action.Write_file (fn, s)))

let write_file_dyn fn s =
  with_targets ~targets:[ fn ]
    (let+ s = s in
     Action.Write_file (fn, s))

let copy ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Copy (src, dst)))

let copy_and_add_line_directive ~src ~dst =
  with_targets ~targets:[ dst ]
    (path src >>> return (Action.Copy_and_add_line_directive (src, dst)))

let symlink ~src ~dst =
  with_targets ~targets:[ dst ] (path src >>> return (Action.Symlink (src, dst)))

let create_file fn =
  with_targets ~targets:[ fn ]
    (return (Action.Redirect_out (Stdout, fn, Action.empty)))

let progn ts =
  let open With_targets.O in
  let+ actions = With_targets.all ts in
  Action.Progn actions

(* Analysis *)

module rec Analysis : sig
  val static_deps : _ t -> Static_deps.t
end = struct
  module Input = struct
    type t = T : _ memo -> t

    let equal (T x) (T y) = Type_eq.Id.equal x.id y.id

    let hash (T m) = Type_eq.Id.hash m.id

    let to_dyn (T m) = Dyn.String m.name
  end

  let memo =
    Memo.create_hidden "Action_builder.static_deps"
      ~input:(module Input)
      Sync
      (fun (T m) -> Analysis.static_deps m.t)

  let rec static_deps : type a. a t -> Static_deps.t =
   fun t ->
    let file_exists = Fdecl.get file_exists_fdecl in
    match t with
    | Pure _ -> Static_deps.empty
    | Map (_, t) -> static_deps t
    | Both (x, y) -> Static_deps.union (static_deps x) (static_deps y)
    | Seq (x, y) -> Static_deps.union (static_deps x) (static_deps y)
    | Map2 (_, x, y) -> Static_deps.union (static_deps x) (static_deps y)
    | All xs -> Static_deps.union_map ~f:static_deps xs
    | Deps deps -> { Static_deps.empty with action_deps = deps }
    | Paths_for_rule fns ->
      { Static_deps.empty with rule_deps = Dep.Set.of_files_set fns }
    | Paths_glob g ->
      { Static_deps.empty with
        action_deps = Dep.Set.singleton (Dep.file_selector g)
      }
    | If_file_exists (p, then_, else_) ->
      if file_exists p then
        static_deps then_
      else
        static_deps else_
    | Filter_existing_files p -> static_deps p
    | Dyn_paths t -> static_deps t
    | Dyn_deps t -> static_deps t
    | Contents p ->
      { Static_deps.empty with rule_deps = Dep.Set.of_files [ p ] }
    | Lines_of p ->
      { Static_deps.empty with rule_deps = Dep.Set.of_files [ p ] }
    | Label _ -> Static_deps.empty
    | Or_exn _ -> Static_deps.empty
    | Fail _ -> Static_deps.empty
    | Memo m -> Memo.exec memo (Input.T m)
    | Catch (t, _) -> static_deps t
    | Memo_build _ -> Static_deps.empty
    | Dyn_memo_build b -> static_deps b
    | Build b -> static_deps b
end

let static_deps = Analysis.static_deps

(* We do no memoization in this function because it is currently used only to
   support the [external-lib-deps] command and so it's not on the critical path. *)
let fold_labeled (type acc) t ~(init : acc) ~f =
  let file_exists = Fdecl.get file_exists_fdecl in
  let rec loop : type a. a t -> acc -> acc =
   fun t acc ->
    match t with
    | Pure _ -> acc
    | Map (_, a) -> loop a acc
    | Both (a, b) ->
      let acc = loop a acc in
      loop b acc
    | Seq (a, b) ->
      let acc = loop a acc in
      loop b acc
    | Map2 (_, a, b) ->
      let acc = loop a acc in
      loop b acc
    | All xs -> List.fold_left xs ~init:acc ~f:(fun acc a -> loop a acc)
    | Paths_for_rule _ -> acc
    | Paths_glob _ -> acc
    | Deps _ -> acc
    | Dyn_paths t -> loop t acc
    | Dyn_deps t -> loop t acc
    | Contents _ -> acc
    | Lines_of _ -> acc
    | Label r -> f r acc
    | Or_exn _ -> acc
    | Fail _ -> acc
    | If_file_exists (p, then_, else_) ->
      if file_exists p then
        loop then_ acc
      else
        loop else_ acc
    | Filter_existing_files p -> loop p acc
    | Memo m -> loop m.t acc
    | Catch (t, _) -> loop t acc
    | Memo_build _ -> acc
    | Dyn_memo_build b -> loop b acc
    | Build b -> loop b acc
  in
  loop t init

(* Execution *)

module Expert = struct
  let action_builder f = Build f
end

let memo_build f = Memo_build f

let dyn_memo_build f = Dyn_memo_build f

module Make_exec (Build_deps : sig
  val build_deps : Dep.Set.t -> unit Memo.Build.t
end) =
struct
  module rec Execution : sig
    val exec : 'a t -> ('a * Dep.Set.t) Memo.Build.t

    val build_static_rule_deps_and_exec : 'a t -> ('a * Dep.Set.t) Memo.Build.t
  end = struct
    module Function = struct
      type 'a input = 'a memo

      type 'a output = 'a * Dep.Set.t

      let name = "exec-memo"

      let id m = m.id

      let to_dyn m = Dyn.String m.name

      let eval m = Execution.exec m.t
    end

    module Poly_memo = Memo.Poly.Async (Function)

    let file_exists x = Fdecl.get file_exists_fdecl x

    let eval_pred x = Fdecl.get Dep.eval_pred x

    open Memo.Build.O

    let rec exec : type a. a t -> (a * Dep.Set.t) Memo.Build.t =
     fun t ->
      match t with
      | Pure x -> Memo.Build.return (x, Dep.Set.empty)
      | Map (f, a) ->
        let+ a, dyn_deps_a = exec a in
        (f a, dyn_deps_a)
      | Both (a, b) ->
        let+ (a, dyn_deps_a), (b, dyn_deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        ((a, b), Dep.Set.union dyn_deps_a dyn_deps_b)
      | Seq (a, b) ->
        let+ ((), dyn_deps_a), (b, dyn_deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        (b, Dep.Set.union dyn_deps_a dyn_deps_b)
      | Map2 (f, a, b) ->
        let+ (a, dyn_deps_a), (b, dyn_deps_b) =
          Memo.Build.fork_and_join (fun () -> exec a) (fun () -> exec b)
        in
        (f a b, Dep.Set.union dyn_deps_a dyn_deps_b)
      | All xs ->
        let+ res = Memo.Build.parallel_map xs ~f:exec in
        let res, deps = List.split res in
        (res, Dep.Set.union_all deps)
      | Deps _ -> Memo.Build.return ((), Dep.Set.empty)
      | Paths_for_rule _ -> Memo.Build.return ((), Dep.Set.empty)
      | Paths_glob g ->
        Memo.Build.return ((eval_pred g : Path.Set.t), Dep.Set.empty)
      | Contents p -> Memo.Build.return (Io.read_file p, Dep.Set.empty)
      | Lines_of p -> Memo.Build.return (Io.lines_of_file p, Dep.Set.empty)
      | Dyn_paths t ->
        let+ (x, paths), dyn_deps = exec t in
        (x, Dep.Set.add_paths dyn_deps paths)
      | Dyn_deps t ->
        let+ (x, dyn_deps), dyn_deps_x = exec t in
        (x, Dep.Set.union dyn_deps dyn_deps_x)
      | Label _ -> Memo.Build.return ((), Dep.Set.empty)
      | Or_exn e ->
        let+ a, deps = exec e in
        (Result.ok_exn a, deps)
      | Fail { fail } -> fail ()
      | If_file_exists (p, then_, else_) ->
        if file_exists p then
          exec then_
        else
          exec else_
      | Filter_existing_files p ->
        let+ (x, files), dyn_deps = exec p in
        let files = Path.Set.filter ~f:file_exists files in
        ((x, files), dyn_deps)
      | Catch (t, on_error) -> (
        let+ res =
          Memo.Build.fold_errors ~init:on_error
            ~on_error:(fun _ x -> x)
            (fun () -> exec t)
        in
        match res with
        | Ok r -> r
        | Error r -> (r, Dep.Set.empty))
      | Memo m -> Poly_memo.eval m
      | Memo_build f ->
        let+ f = f in
        (f, Dep.Set.empty)
      | Dyn_memo_build f ->
        let* f, deps = exec f in
        let+ f = f in
        (f, deps)
      | Build b ->
        let* b, deps0 = exec b in
        let+ r, deps1 = build_static_rule_deps_and_exec b in
        (r, Dep.Set.union deps0 deps1)

    and build_static_rule_deps_and_exec :
        type a. a t -> (a * Dep.Set.t) Memo.Build.t =
     fun t ->
      let* () = Build_deps.build_deps (static_deps t).rule_deps in
      exec t
  end

  include Execution
end
