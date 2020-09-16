open! Stdune

module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs  (** Both Stdout and Stderr *)
end

module Inputs = struct
  type t = Stdin
end

module type Ast = sig
  type program

  type path

  type target

  type string

  type t =
    | Run of program * string list
    | With_accepted_exit_codes of int Predicate_lang.t * t
    | Dynamic_run of program * string list
    | Chdir of path * t
    | Setenv of string * string * t
    (* It's not possible to use a build path here since jbuild supports
       redirecting to /dev/null. In [dune] files this is replaced with %{null} *)
    | Redirect_out of Outputs.t * target * t
    | Redirect_in of Inputs.t * path * t
    | Ignore of Outputs.t * t
    | Progn of t list
    | Echo of string list
    | Cat of path
    | Copy of path * target
    | Symlink of path * target
    | Copy_and_add_line_directive of path * target
    | System of string
    | Bash of string
    | Write_file of target * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of path
    | Digest_files of path list
    | Diff of (path, target) Diff.t
    | Merge_files_into of path list * string list * target
    | No_infer of t
    | Using_terminal of t
    | Pipe of Outputs.t * t list
    | Format_dune_file of path * target
    | Cram of path
end

module type Helpers = sig
  type program

  type path

  type target

  type string

  type t

  val run : program -> string list -> t

  val chdir : path -> t -> t

  val setenv : string -> string -> t -> t

  val with_stdout_to : target -> t -> t

  val with_stderr_to : target -> t -> t

  val with_outputs_to : target -> t -> t

  val with_stdin_from : path -> t -> t

  val ignore_stdout : t -> t

  val ignore_stderr : t -> t

  val ignore_outputs : t -> t

  val progn : t list -> t

  val echo : string list -> t

  val cat : path -> t

  val copy : path -> target -> t

  val symlink : path -> target -> t

  val copy_and_add_line_directive : path -> target -> t

  val system : string -> t

  val bash : string -> t

  val write_file : target -> string -> t

  val rename : target -> target -> t

  val remove_tree : target -> t

  val mkdir : path -> t

  val digest_files : path list -> t

  val diff : ?optional:bool -> ?mode:Diff.Mode.t -> path -> target -> t

  val format_dune_file : path -> target -> t
end
