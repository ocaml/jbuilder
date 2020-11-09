module Appendable_list = Appendable_list
module Ansi_color = Ansi_color
module Array = Array
module Bytes = Bytes
module Char = Char
module Comparator = Comparator
module Console = Console
module Csexp = Csexp.Make (Sexp)
module Daemonize = Daemonize
module Either = Either
module Exn = Exn
module Exn_with_backtrace = Exn_with_backtrace
module Fcntl = Fcntl
module Filename = Filename
module Hashtbl = Hashtbl
module Table = Table
module Int = Int
module Id = Id
module Io = Io
module List = List
module Lock_file = Lock_file
module Map = Map
module Option = Option
module Or_exn = Or_exn
module Ordering = Ordering
module Pp = Pp
module Result = Result
module Set = Set
module Signal = Signal
module Comparable = Comparable
module Comparable_intf = Comparable_intf
module Staged = Staged
module String = String
module Bool = Bool
module Sexp = Sexp
module Path = Path
module Interned = Interned
module Interned_intf = Interned_intf
module Univ_map = Univ_map
module Loc = Loc
module Env = Env
module Proc = Proc
module Type_eq = Type_eq
module Nothing = Nothing
module Bin = Bin
module Digest = Digest
module Fdecl = Fdecl
module Unit = Unit
module Monad = Monad
module Dyn = Dyn
module Float = Float
module Tuple = Tuple
module Poly = Poly
module Code_error = Code_error
module User_error = User_error
module User_message = User_message
module User_warning = User_warning
module Lexbuf = Lexbuf
module Scanf = Scanf
module Sys = Sys
module Pid = Pid
module Applicative_intf = Applicative_intf
module Monad_intf = Monad_intf
module Applicative = Applicative
module Top_closure_intf = Top_closure_intf
module Top_closure = Top_closure
module Spawn = Spawn
module Seq = Seq
module Temp = Temp
module Queue = Queue

external reraise : exn -> _ = "%reraise"

let compare _ _ = `Use_Poly_compare

(* The following types are re-exported here so that they are always available in
   scope *)

type ('a, 'error) result = ('a, 'error) Result.t =
  | Ok of 'a
  | Error of 'error

type ('a, 'b) either = ('a, 'b) Either.t =
  | Left of 'a
  | Right of 'b

type ordering = Ordering.t =
  | Lt
  | Eq
  | Gt

let sprintf = Printf.sprintf

let ksprintf = Printf.ksprintf
