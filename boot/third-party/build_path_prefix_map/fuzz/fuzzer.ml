let roundtrip_from_string encode decode input =
  match decode input with
  | Ok value ->
    let recoded = encode value in
    let pp ppf = Format.fprintf ppf "%S" in
    Crowbar.check_eq ~pp input recoded
  | Error _  ->
    Crowbar.bad_test ()

let roundtrip_from_value pp encode decode input =
  let encoded = encode input in
  match decode encoded with
  | Error _ ->
    Crowbar.check false
  | Ok result ->
    Crowbar.check_eq ~pp input result

module Generators = struct
  open Build_path_prefix_map
  open Crowbar
  let gen_path_prefix = bytes
  let gen_pair =
    let pair source target = {source; target} in
    map [gen_path_prefix; gen_path_prefix] pair
  let gen_map =
    list1 (option gen_pair)
    (* Note: [] and [None] get encoded to the same value,
       and this is the only case of non-injective encoding.
       We use `list1` here to restore the roudtrip property. *)
end

module Printers = struct
  open Format
  open Build_path_prefix_map
  let print_path_prefix ppf prefix = fprintf ppf "%S" prefix
  let print_pair ppf {target; source} =
    fprintf ppf
      "@[{target@ =@ %a;@, source@ =@ %a}@]"
      print_path_prefix target
      print_path_prefix source
  let print_map ppf map =
    let print ppf = function
      | None -> fprintf ppf "None"
      | Some pair -> fprintf ppf "Some %a" print_pair pair
    in
    let pp_sep ppf () = fprintf ppf ";@ " in
    fprintf ppf "@[<2>%a@]" (pp_print_list ~pp_sep print) map
end

let add_roundtrips value_name printer gen encode decode =
  let roundtrip outer inner = Printf.sprintf "%s->%s->%s" outer inner outer in
  let open Crowbar in
  add_test ~name:(roundtrip "string" value_name) [bytes]
    (roundtrip_from_string encode decode);
  add_test ~name:(roundtrip value_name "string") [gen]
    (roundtrip_from_value printer encode decode);
  ()

let () =
  let open Crowbar in
  let open Build_path_prefix_map in
  add_roundtrips "path_prefix"
    (Printers.print_path_prefix : path_prefix printer) Generators.gen_path_prefix
    encode_prefix decode_prefix;
  add_roundtrips "pair"
    Printers.print_pair Generators.gen_pair
    encode_pair decode_pair;
  add_roundtrips "map"
    Printers.print_map Generators.gen_map
    encode_map decode_map;
  ()
