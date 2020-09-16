open Stdune

let terminal_lock = Fiber.Mutex.create ()

module Backend = struct
  module type S = sig
    val print_user_message : User_message.t -> unit

    val set_status_line : User_message.Style.t Pp.t option -> unit

    val reset : unit -> unit
  end

  type t = (module S)

  module Dumb_no_flush : S = struct
    let print_user_message msg =
      Option.iter msg.User_message.loc ~f:(fun loc ->
          Loc.render Format.err_formatter (Loc.pp loc));
      User_message.prerr { msg with loc = None }

    let set_status_line _ = ()

    let reset () = prerr_string "\x1bc"
  end

  module Dumb : S = struct
    include Dumb_no_flush

    let print_user_message msg =
      print_user_message msg;
      flush stderr

    let reset () =
      reset ();
      flush stderr
  end

  module Progress : S = struct
    let status_line = ref Pp.nop

    let status_line_len = ref 0

    let hide_status_line () =
      if !status_line_len > 0 then Printf.eprintf "\r%*s\r" !status_line_len ""

    let show_status_line () =
      if !status_line_len > 0 then Ansi_color.prerr !status_line

    let set_status_line = function
      | None ->
        hide_status_line ();
        status_line := Pp.nop;
        status_line_len := 0;
        flush stderr
      | Some line ->
        let line = Pp.map_tags line ~f:User_message.Print_config.default in
        let line_len =
          String.length (Format.asprintf "%a" Pp.render_ignore_tags line)
        in
        hide_status_line ();
        status_line := line;
        status_line_len := line_len;
        show_status_line ();
        flush stderr

    let print_user_message msg =
      hide_status_line ();
      Dumb_no_flush.print_user_message msg;
      show_status_line ();
      flush stderr

    let reset () = Dumb.reset ()
  end

  module Buffered = struct
    type event =
      | Message of User_message.t
      | Reset

    let underlying = ref (module Dumb : S)

    let buffer : event Queue.t = Queue.create ()

    let print_user_message msg = Queue.push buffer (Message msg)

    let set_status_line _ = ()

    (* I'm unsure of what the behavior should be here. My gut tells me that it
       is less surprising to do nothing (and thus preserving the entire log),
       but it may also be more correct to render the buffered outbut in its
       entirety, resets and all. *)
    let reset () = Queue.push buffer Reset

    let restore () =
      let (module P : S) = !underlying in
      Queue.iter buffer ~f:(function
        | Message msg -> P.print_user_message msg
        | Reset -> P.reset ());
      Queue.clear buffer
  end

  let dumb = (module Dumb : S)

  let progress = (module Progress : S)

  let buffered = (module Buffered : S)

  let main = ref dumb

  let set t = main := t

  let compose (module A : S) (module B : S) =
    ( module struct
      let print_user_message msg =
        A.print_user_message msg;
        B.print_user_message msg

      let set_status_line x =
        A.set_status_line x;
        B.set_status_line x

      let reset () =
        A.reset ();
        B.reset ()
    end : S )
end

let print_user_message msg =
  let (module M : Backend.S) = !Backend.main in
  M.print_user_message msg

let print paragraphs = print_user_message (User_message.make paragraphs)

let set_status_line line =
  let (module M : Backend.S) = !Backend.main in
  M.set_status_line line

let reset () =
  let (module M : Backend.S) = !Backend.main in
  M.reset ()

let lock () =
  Backend.Buffered.underlying := !Backend.main;
  Backend.main := Backend.buffered

let unlock () =
  Backend.Buffered.restore ();
  Backend.main := !Backend.Buffered.underlying

let with_terminal_lock f =
  let open Fiber.O in
  Fiber.Mutex.with_lock terminal_lock (fun () ->
      lock ();
      try
        let* result = f () in
        unlock ();
        Fiber.return result
      with e ->
        unlock ();
        raise e)

module Status_line = struct
  type t = unit -> User_message.Style.t Pp.t option

  let status_line = ref (Fun.const None)

  let refresh () =
    match !status_line () with
    | None -> set_status_line None
    | Some pp ->
      (* Always put the status line inside a horizontal to force the [Format]
         module to prefer a single line. In particular, it seems that
         [Format.pp_print_text] splits the line before the last word, unless it
         is succeeded by a space. This seems like a bug in [Format] and putting
         the whole thing into a [hbox] works around this bug.

         See https://github.com/ocaml/dune/issues/2779 *)
      set_status_line (Some (Pp.hbox pp))

  let set x =
    status_line := x;
    refresh ()

  let set_temporarily x f =
    let old = !status_line in
    set x;
    Exn.protect ~finally:(fun () -> set old) ~f
end

let () = User_warning.set_reporter print_user_message
