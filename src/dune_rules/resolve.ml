open Stdune
open Dune_engine

type error =
  { exn : exn
  ; stack_frames : User_message.Style.t Pp.t Lazy.t list
  }

type 'a t = ('a, error) result

let return x = Ok x

let bind = Result.bind

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return = return

  let bind = bind
end)

let error_equal { exn; stack_frames } b =
  Exn.equal exn b.exn
  && List.equal
       (fun (lazy a) (lazy b) -> Poly.equal a b)
       stack_frames b.stack_frames

let equal f = Result.equal f error_equal

let error_hash { exn; stack_frames } =
  Poly.hash (Exn.hash exn, List.map stack_frames ~f:Lazy.force)

let to_dyn f t =
  Result.to_dyn f Exn.to_dyn (Result.map_error t ~f:(fun x -> x.exn))

let hash f = Result.hash f error_hash

let of_result = Result.map_error ~f:(fun exn -> { exn; stack_frames = [] })

let error_to_memo_build { stack_frames; exn } =
  let open Memo.Build.O in
  let rec loop = function
    | [] ->
      let+ () = Memo.Build.return () in
      raise exn
    | x :: rest ->
      Memo.push_stack_frame
        ~human_readable_description:(fun () -> Lazy.force x)
        (fun () -> loop rest)
  in
  loop stack_frames

let read_memo_build = function
  | Ok x -> Memo.Build.return x
  | Error err -> error_to_memo_build err

let read = function
  | Ok x -> Action_builder.return x
  | Error err -> Action_builder.memo_build (error_to_memo_build err)

let args t =
  match t with
  | Ok args -> args
  | Error _ ->
    let open Action_builder.O in
    Command.Args.Dyn (read t >>| fun _ -> assert false)

let fail msg =
  Error
    { exn = User_error.E (msg, User_error.Annotations.none); stack_frames = [] }

let peek t = Result.map_error t ~f:ignore

let is_ok t = Result.is_ok (peek t)

let is_error t = Result.is_error (peek t)

let push_stack_frame ~human_readable_description:f t =
  match t with
  | Ok _ -> t
  | Error err ->
    Error { err with stack_frames = Lazy.from_fun f :: err.stack_frames }

module Build = struct
  let bind t ~f =
    match t with
    | Error _ as err -> Memo.Build.return err
    | Ok x -> Memo.Build.map (f x) ~f:Fun.id
end

module List = struct
  let map = Result.List.map

  let filter_map = Result.List.filter_map

  let concat_map = Result.List.concat_map

  let iter = Result.List.iter

  let fold_left = Result.List.fold_left
end

let all = List.map ~f:Fun.id

module Option = struct
  let iter x ~f =
    match x with
    | None -> return ()
    | Some x -> f x
end
