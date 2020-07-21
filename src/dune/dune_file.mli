(** Representation and parsing of Dune files *)

open! Stdune
open Import

module Lint : sig
  type t = Preprocess.Without_instrumentation.t Preprocess.Per_module.t

  val no_lint : t
end

module Js_of_ocaml : sig
  type t =
    { flags : Ordered_set_lang.Unexpanded.t
    ; javascript_files : string list
    }

  val default : t
end

module Lib_deps : sig
  type nonrec t = Lib_dep.t list

  val of_pps : Lib_name.t list -> t

  val info : t -> kind:Lib_deps_info.Kind.t -> Lib_deps_info.t

  val decode : allow_re_export:bool -> t Dune_lang.Decoder.t
end

(** [preprocess] and [preprocessor_deps] fields *)
val preprocess_fields :
  ( Preprocess.Without_instrumentation.t Preprocess.Per_module.t
  * Dep_conf.t list )
  Dune_lang.Decoder.fields_parser

module Buildable : sig
  type t =
    { loc : Loc.t
    ; modules : Ordered_set_lang.t
    ; modules_without_implementation : Ordered_set_lang.t
    ; libraries : Lib_dep.t list
    ; foreign_archives : (Loc.t * Foreign.Archive.t) list
    ; foreign_stubs : Foreign.Stubs.t list
    ; preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
    ; preprocessor_deps : Dep_conf.t list
    ; lint : Lint.t
    ; flags : Ocaml_flags.Spec.t
    ; js_of_ocaml : Js_of_ocaml.t
    ; allow_overlapping_dependencies : bool
    }

  (** Check if the buildable has any foreign stubs or archives. *)
  val has_foreign : t -> bool
end

module Public_lib : sig
  type t

  (** Subdirectory inside the installation directory *)
  val sub_dir : t -> string option

  val loc : t -> Loc.t

  (** Full public name *)
  val name : t -> Lib_name.t

  (** Package it is part of *)
  val package : t -> Package.t
end

module Mode_conf : sig
  type t =
    | Byte
    | Native
    | Best  (** [Native] if available and [Byte] if not *)

  val decode : t Dune_lang.Decoder.t

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  module Kind : sig
    type t =
      | Inherited
      | Requested of Loc.t
  end

  module Map : sig
    type nonrec 'a t =
      { byte : 'a
      ; native : 'a
      ; best : 'a
      }
  end

  module Set : sig
    type nonrec t = Kind.t option Map.t

    val decode : t Dune_lang.Decoder.t

    module Details : sig
      type t = Kind.t option
    end

    val eval_detailed : t -> has_native:bool -> Details.t Mode.Dict.t

    val eval : t -> has_native:bool -> Mode.Dict.Set.t
  end
end

module Library : sig
  type t =
    { name : Loc.t * Lib_name.Local.t
    ; public : Public_lib.t option
    ; synopsis : string option
    ; install_c_headers : string list
    ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
    ; modes : Mode_conf.Set.t
    ; kind : Lib_kind.t
          (* TODO: It may be worth remaming [c_library_flags] to
             [link_time_flags_for_c_compiler] and [library_flags] to
             [link_time_flags_for_ocaml_compiler], both here and in the Dune
             language, to make it easier to understand the purpose of various
             flags. Also we could add [c_library_flags] to [Foreign.Stubs.t]. *)
    ; library_flags : Ordered_set_lang.Unexpanded.t
    ; c_library_flags : Ordered_set_lang.Unexpanded.t
    ; virtual_deps : (Loc.t * Lib_name.t) list
    ; wrapped : Wrapped.t Lib_info.Inherited.t
    ; optional : bool
    ; buildable : Buildable.t
    ; dynlink : Dynlink_supported.t
    ; project : Dune_project.t
    ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
    ; dune_version : Dune_lang.Syntax.Version.t
    ; virtual_modules : Ordered_set_lang.t option
    ; implements : (Loc.t * Lib_name.t) option
    ; default_implementation : (Loc.t * Lib_name.t) option
    ; private_modules : Ordered_set_lang.t option
    ; stdlib : Ocaml_stdlib.t option
    ; special_builtin_support : Lib_info.Special_builtin_support.t option
    ; enabled_if : Blang.t
    ; instrumentation_backend : (Loc.t * Lib_name.t) option
    }

  (** Check if the library has any foreign stubs or archives. *)
  val has_foreign : t -> bool

  (** The list of all foreign archives, including the foreign stubs archive. *)
  val foreign_archives : t -> Foreign.Archive.t list

  (** The [lib*.a] files of all foreign archives, including foreign stubs. [dir]
      is the directory the library is declared in. *)
  val foreign_lib_files :
    t -> dir:Path.Build.t -> ext_lib:string -> Path.Build.t list

  (** The [dll*.so] files of all foreign archives, including foreign stubs.
      [dir] is the directory the library is declared in. *)
  val foreign_dll_files :
    t -> dir:Path.Build.t -> ext_dll:string -> Path.Build.t list

  (** The path to a library archive. [dir] is the directory the library is
      declared in. *)
  val archive : t -> dir:Path.Build.t -> ext:string -> Path.Build.t

  val best_name : t -> Lib_name.t

  val is_virtual : t -> bool

  val is_impl : t -> bool

  val obj_dir : dir:Path.Build.t -> t -> Path.Build.t Obj_dir.t

  val main_module_name : t -> Lib_info.Main_module_name.t

  val to_lib_info :
    t -> dir:Path.Build.t -> lib_config:Lib_config.t -> Lib_info.local
end

module Install_conf : sig
  type t =
    { section : Install.Section.t
    ; files : File_binding.Unexpanded.t list
    ; package : Package.t
    ; enabled_if : Blang.t
    }
end

module Executables : sig
  module Link_mode : sig
    type t =
      | Byte_complete
      | Other of
          { mode : Mode_conf.t
          ; kind : Binary_kind.t
          }

    include Dune_lang.Conv.S with type t := t

    val exe : t

    val object_ : t

    val shared_object : t

    val byte : t

    val native : t

    val js : t

    val compare : t -> t -> Ordering.t

    val to_dyn : t -> Dyn.t

    val extension : t -> loc:Loc.t -> ext_obj:string -> ext_dll:string -> string

    module Map : Map.S with type key = t
  end

  type t =
    { names : (Loc.t * string) list
    ; link_flags : Ordered_set_lang.Unexpanded.t
    ; link_deps : Dep_conf.t list
    ; modes : Loc.t Link_mode.Map.t
    ; optional : bool
    ; buildable : Buildable.t
    ; package : Package.t option
    ; promote : Rule.Promote.t option
    ; install_conf : Install_conf.t option
    ; embed_in_plugin_libraries : (Loc.t * Lib_name.t) list
    ; forbidden_libraries : (Loc.t * Lib_name.t) list
    ; bootstrap_info : string option
    ; enabled_if : Blang.t
    }

  (** Check if the executables have any foreign stubs or archives. *)
  val has_foreign : t -> bool

  val obj_dir : t -> dir:Path.Build.t -> Path.Build.t Obj_dir.t
end

module Menhir : sig
  type t =
    { merge_into : string option
    ; flags : Ordered_set_lang.Unexpanded.t
    ; modules : string list
    ; mode : Rule.Mode.t
    ; loc : Loc.t
    ; infer : bool
    ; enabled_if : Blang.t
    }

  type Stanza.t += T of t
end

module Copy_files : sig
  type t =
    { add_line_directive : bool
    ; alias : Alias.Name.t option
    ; mode : Rule.Mode.t
    ; files : String_with_vars.t
    ; syntax_version : Dune_lang.Syntax.Version.t
    }
end

module Copy_dir : sig
  type t = { path : String_with_vars.t }
end

module Rule : sig
  type t =
    { targets : String_with_vars.t Targets.t
    ; deps : Dep_conf.t Bindings.t
    ; action : Loc.t * Action_dune_lang.t
    ; mode : Rule.Mode.t
    ; locks : String_with_vars.t list
    ; loc : Loc.t
    ; enabled_if : Blang.t
    ; alias : Alias.Name.t option
    ; package : Package.t option
    }
end

module Alias_conf : sig
  type t =
    { name : Alias.Name.t
    ; deps : Dep_conf.t Bindings.t
    ; action : (Loc.t * Action_dune_lang.t) option
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; enabled_if : Blang.t
    ; loc : Loc.t
    }
end

module Documentation : sig
  type t =
    { loc : Loc.t
    ; package : Package.t
    ; mld_files : Ordered_set_lang.t
    }
end

module Tests : sig
  type t =
    { exes : Executables.t
    ; locks : String_with_vars.t list
    ; package : Package.t option
    ; deps : Dep_conf.t Bindings.t
    ; enabled_if : Blang.t
    ; action : Action_dune_lang.t option
    }
end

module Toplevel : sig
  type t =
    { name : string
    ; libraries : (Loc.t * Lib_name.t) list
    ; loc : Loc.t
    ; pps : Preprocess.Without_instrumentation.t Preprocess.t
    }
end

module Include_subdirs : sig
  type qualification =
    | Unqualified
    | Qualified

  type t =
    | No
    | Include of qualification
end

module Deprecated_library_name : sig
  module Old_public_name : sig
    type kind =
      | Not_deprecated
      | Deprecated of { deprecated_package : Package.Name.t }

    type t =
      { kind : kind
      ; public : Public_lib.t
      }
  end

  type t =
    { loc : Loc.t
    ; project : Dune_project.t
    ; old_public_name : Old_public_name.t
    ; new_public_name : Loc.t * Lib_name.t
    }
end

type Stanza.t +=
  | Library of Library.t
  | Foreign_library of Foreign.Library.t
  | Executables of Executables.t
  | Rule of Rule.t
  | Install of Install_conf.t
  | Alias of Alias_conf.t
  | Copy_files of Copy_files.t
  | Copy_dir of Copy_dir.t
  | Documentation of Documentation.t
  | Tests of Tests.t
  | Include_subdirs of Loc.t * Include_subdirs.t
  | Toplevel of Toplevel.t
  | Deprecated_library_name of Deprecated_library_name.t

val stanza_package : Stanza.t -> Package.t option

module Stanzas : sig
  type t = Stanza.t list

  type syntax =
    | OCaml
    | Plain

  (** [of_ast project ast] is the list of [Stanza.t]s derived from decoding the
      [ast] according to the syntax given by [kind] in the context of the
      [project] *)
  val of_ast : Dune_project.t -> Dune_lang.Ast.t -> Stanza.t list

  (** [parse ~file ~kind project stanza_exprs] is a list of [Stanza.t]s derived
      from decoding the [stanza_exprs] from [Dune_lang.Ast.t]s to [Stanza.t]s.

      [file] is used to check for illegal recursive file inclusions and to
      anchor file includes given as relative paths.

      The stanzas are parsed in the context of the dune [project].

      The syntax [kind] determines whether the expected syntax is the
      depreciated jbuilder syntax or the version of Dune syntax specified by the
      current [project]. *)
  val parse : file:Path.Source.t -> Dune_project.t -> Dune_lang.Ast.t list -> t
end
