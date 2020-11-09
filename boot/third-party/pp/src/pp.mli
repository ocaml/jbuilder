(** Pretty-printing. *)

(** A document that is not yet rendered. The argument is the type of tags in the
    document. For instance tags might be used for styles. *)
type +'tag t

(** {1 Basic combinators} *)

(** A pretty printer that prints nothing *)
val nop : _ t

(** [seq x y] prints [x] and then [y] *)
val seq : 'a t -> 'a t -> 'a t

(** [concat ?sep l] prints elements in [l] separated by [sep]. [sep] defaults to
    [nop]. *)
val concat : ?sep:'a t -> 'a t list -> 'a t

(** Convenience function for [List.map] followed by [concat] *)
val concat_map : ?sep:'a t -> 'b list -> f:('b -> 'a t) -> 'a t

val concat_mapi : ?sep:'a t -> 'b list -> f:(int -> 'b -> 'a t) -> 'a t

(** An indivisible block of text *)
val verbatim : string -> _ t

(** A single character *)
val char : char -> _ t

(** Print a bunch of text. The line may be broken at any spaces in the text. *)
val text : string -> _ t

(** Same as [text] but take a format string as argument. *)
val textf : ('a, unit, string, _ t) format4 -> 'a

(** {1 Break hints} *)

(** [space] instructs the pretty-printing algorithm that the line may be broken
    at this point. If the algorithm decides not to break the line, a single
    space will be printed instead.

    So for instance [verbatim "x" ++ space ++ verbatim "y"] might produce "x y"
    or "x\n<indentation>y". *)
val space : _ t

(** [cut] instructs the pretty-printing algorithm that the line may be broken at
    this point. If the algorithm decides not to break the line, nothing is
    printed instead.

    So for instance [verbatim "x" ++ space ++ verbatim "y"] might produce "xy"
    or "x\n<indentation>y". *)
val cut : _ t

(** [break] is a generalisation of [space] and [cut]. It also instructs the
    pretty-printing algorithm that the line may be broken at this point. If it
    ends up being broken, [shift] will be added to the indentation level,
    otherwise [nspaces] spaces will be printed. [shift] can be negative, in
    which case the indentation will be reduced. *)
val break : nspaces:int -> shift:int -> _ t

(** [custom_break ~fits:(a, b, c) ~breaks:(x, y, z)] is a generalisation of
    [break]. It also instructs the pretty-printing algorithm that the line may
    be broken at this point. If it ends up being broken, [x] is printed, the
    line breaks, [y] will be added to the indentation level and [z] is printed,
    otherwise [a] will be printed, [b] spaces are printed and then [c] is
    printed. The indentation [y] can be negative, in which case the indentation
    will be reduced. *)
val custom_break :
  fits:string * int * string -> breaks:string * int * string -> _ t

(** Force a newline to be printed *)
val newline : _ t

(** {1 Boxes} *)

(** Boxes are the basic components to control the layout of the text. Break
    hints such as [space] and [cut] may cause the line to be broken, depending
    on the splitting rules. Whenever a line is split, the rest of the material
    printed in the box is indented with [indent].

    You can think of a box with indentation as something with this shape:

    {v
       ######################### <- first line
       <indent>#################
       <indent>#################
       <indent>#################
       <indent>#################
    v}

    And the top left corner of this shape is anchored where the box was
    declared. So for instance, the following document:

    {[ Pp.verbatim "....." ++ Pp.box ~indent:2 (Pp.text "some long ... text") ]}

    would produce:

    {v
       .....some long ...
              text
    v} *)

(** Try to put as much as possible on each line. Additionally, a break hint
    always break the line if the breaking would reduce the indentation level
    inside the box ([break] with negative [shift] value). *)
val box : ?indent:int -> 'a t -> 'a t

(** Always break the line when encountering a break hint. *)
val vbox : ?indent:int -> 'a t -> 'a t

(** Print everything on one line, no matter what *)
val hbox : 'a t -> 'a t

(** If possible, print everything on one line. Otherwise, behave as a [vbox] *)
val hvbox : ?indent:int -> 'a t -> 'a t

(** Try to put as much as possible on each line. Basically the same as [box] but
    without the rule about breaks with negative [shift] value. *)
val hovbox : ?indent:int -> 'a t -> 'a t

(** {1 Tags} *)

(** Tags are arbitrary pieces of information attached to a document. They can be
    used to add styles to pretty-printed text, for instance to print to the
    terminal with colors. *)

(** [tag x t] Tag the material printed by [t] with [x] *)
val tag : 'a -> 'a t -> 'a t

(** Convert tags in a documents *)
val map_tags : 'a t -> f:('a -> 'b) -> 'b t

val filter_map_tags : 'a t -> f:('a -> 'b option) -> 'b t

(** {1 Convenience functions} *)

(** [enumerate l ~f] produces an enumeration of the form:

    {v
      - item1
      - item2
      - item3
      ...
    v} *)
val enumerate : 'a list -> f:('a -> 'b t) -> 'b t

(** [chain l ~f] is used to print a succession of items that follow each other.
    It produces an output of this form:

    {v
         item1
      -> item2
      -> item3
      ...
    v} *)
val chain : 'a list -> f:('a -> 'b t) -> 'b t

(** {1 Operators} *)

module O : sig
  (** Same as [seq] *)
  val ( ++ ) : 'a t -> 'a t -> 'a t
end

(** {1 Rendering} *)

(** Render a document to a classic formatter *)
val to_fmt : Format.formatter -> _ t -> unit

val to_fmt_with_tags :
     Format.formatter
  -> 'a t
  -> tag_handler:(Format.formatter -> 'a -> 'a t -> unit)
  -> unit
