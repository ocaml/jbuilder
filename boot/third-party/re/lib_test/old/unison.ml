let desc =
  [ `Path "lablgtk-1.00/config.make";
    `Path "lablgtk-1.00/lablgtktop_t";
    `Path "lablgtk-1.00/lablgtktop";
    `Path "lablgtk-1.00/lablgtkrun";
    `Path "lablgtk-1.00/lablgtk";
    `Path "unison3/src/unison";
    `Name "core";
    `Path "lipe/caisse/val_parse.h";
    `Path "lipe/caisse/val_parse.c";
    `Path "lipe/caisse/val_lex.c";
    `Path "lipe/caisse/caisse";
    `Path "lipe/runtime";
    `Path "lipe/demo";
    `Path "unison2/doc/unison-manual.ps";
    `Path "unison/doc/unison-mal.ps";
    `Name "*.ppi";
    `Path "unison2/src/unison";
    `Path "Xduce/xduce/pref.ml";
    `Path "Xduce/xduce/xduce{,.opt}";
    `Path "unison/src/TAGS";
    `Path "unison/src/unison";
    `Name "*.old";
    `Name "#*#";
    `Name "*.cm{i,o,x,a,xa}";
    `Name "*.vo";
    `Name "*{~,.aux,.bbl,.blg,.log,.toc,.o,.a}";
    `Name "gmon.out";
    `Name "ocamlprof.dump";
    `Name "CVS";
    `Name ".*.prcs_aux";
    `Path "icfp2000/tools/src2tex";
    `Path "icfp2000/temp.dvi";
    `Path "icfp2000/main.dvi";
    `Path "icfp2000/whole.dvi";
    `Path "icfp2000/regsub.ps";
    `Path "Views/main.dvi";
    `Path "lipe/perso/caisse";
    `Name "obsolete";
    `Path "misc/fingerprint/cksum/cksum";
    `Path "misc/relay/relay";
    `Path "Xduce/xduce.current/xduce.opt";
    `Path "Xduce/xduce.current/pref.ml";
    `Path "Xduce/xduce.new/pref.ml";
    `Path "Xduce/xduce.new/xduce.opt";
    `Path "profiler/profiler";
    `Path "ocaml/boot/Saved";
    `Path "ocaml/byterun/ocamlrun";
    `Path "ocaml/config/Makefile";
    `Path "ocaml/config/m.h";
    `Path "ocaml/config/s.h";
    `Path "ocaml/expunge";
    `Path "ocaml/asmcomp/arch.ml";
    `Path "ocaml/asmcomp/emit.ml";
    `Path "ocaml/asmcomp/proc.ml";
    `Path "ocaml/asmcomp/reload.ml";
    `Path "ocaml/asmcomp/scheduling.ml";
    `Path "ocaml/asmcomp/selection.ml";
    `Path "ocaml/debugger/ocamldebug";
    `Path "ocaml/lex/ocamllex";
    `Path "ocaml/ocaml";
    `Path "ocaml/ocamlc";
    `Path "ocaml/ocamlopt";
    `Path "ocaml/otherlibs/dynlink/extract_crc";
    `Path "ocaml/otherlibs/labltk/browser/ocamlbrowser";
    `Path "ocaml/otherlibs/labltk/compiler/tkcompiler";
    `Path "ocaml/otherlibs/str/regex-0.12/config.status";
    `Path "ocaml/stdlib/camlheader";
    `Path "ocaml/tools/cvt_emit";
    `Path "ocaml/boot/camlheader";
    `Path "ocaml/boot/ocamlrun";
    `Path "ocaml/boot/ocamlyacc";
    `Path "ocaml/otherlibs/labltk/lib/.depend";
    `Path "ocaml/otherlibs/labltk/lib/labltk";
    `Path "ocaml/otherlibs/labltk/lib/labltktop";
    `Path "ocaml/otherlibs/labltk/lib/tk.ml";
    `Path "ocaml/tools/ocamlcp";
    `Path "ocaml/tools/ocamldep";
    `Path "ocaml/tools/ocamlmktop";
    `Path "ocaml/tools/ocamlprof";
    `Path "ocaml/utils/config.ml";
    `Path "ocaml/yacc/ocamlyacc";
    `Path "Xduce/interleave/tools/src2tex";
    `Path "xml/parser";
    `Path "ocaml/ocamlopt.opt";
    `Path "ocaml/boot/ocamlc";
    `Path "ocaml/boot/ocamllex";
    `Path "ocaml/ocamlc.opt";
    `Path "specs/tools/src2f";
    `Path "specs/tools/src2tex" ]

let paths = 
  [ "These";
    "Xduce";
    "unison";
    "unison2";
    "unison3";
    "tinkertype";
    "lipe";
    "icfp2000";
    "Views";
    "sync";
    "misc";
    "lablgtk-1.00";
    "mydb";
    "yacc";
    "db-papers";
    "submissions";
    "xml";
    "profiler";
    "specs";
    "ocaml";
    "rx" ]

let rec children p =
  let rec loop ch dir =
    try
      let file = Unix.readdir dir in
      let ch' =
        if file = "." || file = ".." then
          ch
        else
          file :: ch in
      loop ch' dir
    with End_of_file ->
      ch
  in
  let dir = Unix.opendir p in
  let result = loop [] dir in
  Unix.closedir dir;
  result

let is_dir p =
  try
    (Unix.lstat p).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error _ ->
    false

let prefix ="/home/jerome/"

let count = ref 0
let hit = ref 0
let rec visit rx p =
(*incr count; if !count > 0 then raise Exit;*)
  try
incr count;
    ignore (Re.exec ~group:false rx p);
(*Format.eprintf "-%s@." p*)
  with Not_found ->
(*Format.eprintf "+%s@." p;*)
    let fp = prefix ^ p in
incr hit;
    if is_dir fp then
      List.iter (fun n -> visit rx (p ^ "/" ^ n)) (children fp)

let _ =
for i = 0 to 9 do
count := 0; hit := 0;
  let rx =
    Re.seq
      [ Re.bos;
        Re.alt
          (List.map
             (fun p ->
                match p with
                  `Path s ->
                     Re.Glob.globx s
                | `Name s ->
                     Re.seq [Re.no_group (Re_posix.re "(.*/)?");
                             Re.Glob.globx s])
             desc);
        Re.eos ]
  in
  let rx = Re.compile rx in
begin try
  List.iter (fun p -> visit rx p) paths
with Exit -> () end;
Format.eprintf "%d/%d@." !hit !count;
done

