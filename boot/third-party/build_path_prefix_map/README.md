This small library is an OCaml implementation of Ximin Luo's
[specification](https://reproducible-builds.org/specs/build-path-prefix-map/)
of the BUILD_PATH_PREFIX_MAP format, used to specify path rewrites for
reproducible builds:

<https://reproducible-builds.org/specs/build-path-prefix-map/>

BUILD_PATH_PREFIX_MAP is a Unix environment variable that users can
set to influence compilers and other build tools (that respect the
proposed specification) in how they record absolute paths to the build
directory, in order to enable reproducible build results from
different build directories.

For context, see the whole [Reproducible
Builds](https://reproducible-builds.org/) project at

  <https://reproducible-builds.org/>

This library implements encoding ({!encode_map}) and decoding
({!decode_map}) of the BUILD_PATH_PREFIX_MAP variable (from a string,
encoded according to the specification, to a prefix map, that is an
ordered list of (target, source) pairs of build path prefixes. It also
provides a rewriting function, {!rewrite}, to transform an absolute
path according to a prefix map.

Below is a simple decoding example:

```ocaml
(* /home/gasche -> home,
   /home/gasche/.opam -> opam *)
let encoded_map = "home=/home/gasche:opam=/home/gasche/.opam"

(* decoding example *)
let () = match Build_path_prefix_map.decode_map encoded_map with
  | Error err -> failwith ("invalid map " ^ encoded_map)
  | Ok map ->
    let transform path = Build_path_prefix_map.rewrite map path in
    assert (transform "/home/gasche/Prog/foo.ml" = "home/Prog/foo.ml");
    assert (transform "/home/gasche/.opam/4.06.0/" = "opam/4.06.0/");
    ()

(* encoding example *)
let decoded_map =
  (* Be careful, the specification explicitly indicates that (source,
     target) pairs are tried from right to left (from the end of the
     list to the beginning), with the first one stopping the
     search. So the order matter: putting the 'opam' pair before the
     'home' pair would make it useless, as all rewrites would stop on
     'home' first. *)
  Build_path_prefix_map.[
    Some {source = "/home/gasche"; target = "home"};
    Some {source = "/home/gasche/.opam"; target = "opam"};
  ]
let () = assert (Build_path_prefix_map.encode_map decoded_map = encoded_map)
```
