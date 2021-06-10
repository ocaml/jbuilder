(** Reading OCaml values from dune lang ones *)

open Stdune

type ast = Ast.t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * ast list

type hint =
  { on : string
  ; candidates : string list
  }

(** Monad producing a value of type ['a] by parsing an input composed of a
    sequence of S-expressions.

    The input can be seen either as a plain sequence of S-expressions or a list
    of fields. The ['kind] parameter indicates how the input is seen:

    - with ['kind = \[values\]], the input is seen as an ordered sequence of
      S-expressions

    - with [!'kind = \[fields\]], the input is seen as an unordered sequence of
      fields

    A field is a S-expression of the form: [(<atom> <values>...)] where [atom]
    is a plain atom, i.e. not a quoted string and not containing variables.
    [values] is a sequence of zero, one or more S-expressions.

    It is possible to switch between the two mode at any time using the
    appropriate combinator. Some primitives can be used in both mode while some
    are specific to one mode. *)
type ('a, 'kind) parser

type values

type fields

type 'a t = ('a, values) parser

type 'a fields_parser = ('a, fields) parser

(** [parse parser context sexp] parse a S-expression using the following parser.
    The input consist of a single S-expression. [context] allows to pass extra
    information such as versions to individual parsers. *)
val parse : 'a t -> Univ_map.t -> ast -> 'a

val set_input : ast list -> (unit, 'k) parser

val return : 'a -> ('a, _) parser

val ( >>= ) : ('a, 'k) parser -> ('a -> ('b, 'k) parser) -> ('b, 'k) parser

val ( >>| ) : ('a, 'k) parser -> ('a -> 'b) -> ('b, 'k) parser

val ( >>> ) : (unit, 'k) parser -> ('a, 'k) parser -> ('a, 'k) parser

val map : ('a, 'k) parser -> f:('a -> 'b) -> ('b, 'k) parser

val try_ : ('a, 'k) parser -> (exn -> ('a, 'k) parser) -> ('a, 'k) parser

val traverse : 'a list -> f:('a -> ('b, 'k) parser) -> ('b list, 'k) parser

val all : ('a, 'k) parser list -> ('a list, 'k) parser

(** Access to the context *)
val get : 'a Univ_map.Key.t -> ('a option, _) parser

val set : 'a Univ_map.Key.t -> 'a -> ('b, 'k) parser -> ('b, 'k) parser

val get_all : (Univ_map.t, _) parser

val set_many : Univ_map.t -> ('a, 'k) parser -> ('a, 'k) parser

val update_var :
     'a Univ_map.Key.t
  -> f:('a option -> 'a option)
  -> ('b, 'k) parser
  -> ('b, 'k) parser

(** Return the location of the list currently being parsed. *)
val loc : (Loc.t, _) parser

(** [a <|> b] is either [a] or [b]. If [a] fails to parse the input, then try
    [b]. If [b] fails as well, raise the error from the parser that consumed the
    most input. *)
val ( <|> ) : ('a, 'k) parser -> ('a, 'k) parser -> ('a, 'k) parser

val either :
  ('a, 'k) parser -> ('b, 'k) parser -> (('a, 'b) Either.t, 'k) parser

(** [atom_matching f] expects the next element to be an atom for which [f]
    returns [Some v]. [desc] is used to describe the atom in case of error. [f]
    must not raise. *)
val atom_matching : (string -> 'a option) -> desc:string -> 'a t

(** [keyword s] is a short-hand for

    {[ atom_matching (String.equal s) ~desc:(sprintf "'%s'" s) ]} *)
val keyword : string -> unit t

(** Use [before] to parse elements until the keyword is reached. Then use
    [after] to parse the rest. *)
val until_keyword :
  string -> before:'a t -> after:'b t -> ('a list * 'b option) t

(** What is currently being parsed. The second argument is the atom at the
    beginning of the list when inside a [sum ...] or [field ...]. *)
type kind =
  | Values of Loc.t * string option
  | Fields of Loc.t * string option

val kind : (kind, _) parser

(** [repeat t] uses [t] to consume all remaining elements of the input until the
    end of sequence is reached. *)
val repeat : 'a t -> 'a list t

(** Like [repeat] but the list of elements must be non-empty. *)
val repeat1 : 'a t -> 'a list t

(** Capture the rest of the input for later parsing *)
val capture : ('a t -> 'a) t

(** Delay the parsing of the rest of the input *)
val lazy_ : 'a t -> 'a Lazy.t t

(** [enter t] expect the next element of the input to be a list and parse its
    contents with [t]. *)
val enter : 'a t -> 'a t

(** [fields fp] converts the rest of the current input to a list of fields and
    parse them with [fp]. This operation fails if one the S-expression in the
    input is not of the form [(<atom> <values>...)] *)
val fields : 'a fields_parser -> 'a t

(** Consume the next element of the input as a string, int, bool, ... *)
val string : string t

val int : int t

val float : float t

val bool : bool t

val pair : 'a t -> 'b t -> ('a * 'b) t

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [maybe t] is a short-hand for:

    {[
      (let+ x = t in
       Some x)
      <|> return None
    ]} *)
val maybe : 'a t -> 'a option t

(** Consume the next element as a duration, requiring 's', 'm' or 'h' suffix *)
val duration : int t

(** Consume the next element as a number of bytes, requiring 'B', 'KB', 'MB' or
    'GB' suffix *)
val bytes_unit : int64 t

(** Unparsed next element of the input *)
val raw : ast t

(** Inspect the next element of the input without consuming it *)
val peek : ast option t

(** Same as [peek] but fail if the end of input is reached *)
val peek_exn : ast t

(** Consume and ignore the next element of the input *)
val junk : unit t

(** Ignore all the rest of the input *)
val junk_everything : (unit, _) parser

(** [plain_string f] expects the next element of the input to be a plain string,
    i.e. either an atom or a quoted string, but not a template nor a list. *)
val plain_string : (loc:Loc.t -> string -> 'a) -> 'a t

(** A valid filename, i.e. a string other than "." or ".." *)
val filename : string t

(** A relative filename *)
val relative_file : string t

val fix : ('a t -> 'a t) -> 'a t

val located : ('a, 'k) parser -> (Loc.t * 'a, 'k) parser

val enum : (string * 'a) list -> 'a t

(** Parser that parse a S-expression of the form
    [(<atom> <s-exp1> <s-exp2> ...)] or [<atom>]. [<atom>] is looked up in the
    list and the remaining s-expressions are parsed using the corresponding list
    parser.

    If [force_parens] is [true], then the form [<atom>] is never accepted. The
    default is [false]. *)
val sum : ?force_parens:bool -> (string * 'a t) list -> 'a t

(** Check the result of a list parser, and raise a properly located error in
    case of failure. *)
val map_validate :
  ('a, 'k) parser -> f:('a -> ('b, User_message.t) Result.t) -> ('b, 'k) parser

(** {3 Parsing record fields} *)

val field :
     string
  -> ?default:'a
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> 'a t
  -> 'a fields_parser

val field_o :
     string
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> 'a t
  -> 'a option fields_parser

(** Parser for mutually exclusive fields. If [default] is provided, allow fields
    absence. *)
val fields_mutually_exclusive :
     ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> ?default:'a
  -> (string * 'a t) list
  -> 'a fields_parser

(** Test if the field is present *)
val field_b :
     ?check:unit t
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> string
  -> bool fields_parser

(** Differentiate between not present and set to true or false *)
val field_o_b :
     ?check:unit t
  -> ?on_dup:(Univ_map.t -> string -> Ast.t list -> unit)
  -> string
  -> bool option fields_parser

(** A field that can appear multiple times *)
val multi_field : string -> 'a t -> 'a list fields_parser

(** Treat the remaining fields as a list of sum values *)
val leftover_fields_as_sums : (string * 'a t) list -> 'a list fields_parser

(** Default value for [on_dup]. It fails with an appropriate error message. *)
val field_present_too_many_times : Univ_map.t -> string -> Ast.t list -> _

val leftover_fields : Ast.t list fields_parser

val ( let* ) : ('a, 'k) parser -> ('a -> ('b, 'k) parser) -> ('b, 'k) parser

val ( let+ ) : ('a, 'k) parser -> ('a -> 'b) -> ('b, 'k) parser

val ( and+ ) : ('a, 'k) parser -> ('b, 'k) parser -> ('a * 'b, 'k) parser

val hack : string  -> ('a, values) parser  -> ('a, values) parser -> ('a, values) parser
