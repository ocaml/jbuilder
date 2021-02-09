open Stdune
open Dune_tests_common

let () = init ()

type mynode = { name : string }

module DagF = Dag

module Dag = struct
  include Dag.Make (struct
    type t = mynode
  end)

  let node dag data = { info = create_node_info dag; data }
end

open Dag

let dag = Dag.create ()

let node = Dag.node dag { name = "root" }

let node11 = Dag.node dag { name = "child 1 1" }

let node12 = Dag.node dag { name = "child 1 2" }

let node21 = Dag.node dag { name = "child 2 1" }

let node31 = Dag.node dag { name = "child 3 1" }

let () =
  Dag.add_assuming_missing dag node node11;
  Dag.add_assuming_missing dag node node12;
  Dag.add_assuming_missing dag node12 node21;
  Dag.add_assuming_missing dag node21 node31

let pp_mynode fmt n = Format.fprintf fmt "%s" n.name

let dag_pp_mynode = Dag.pp_node pp_mynode

let%expect_test _ =
  Format.printf "%a@." dag_pp_mynode node;
  let node41 = Dag.node dag { name = "child 4 1" } in
  Dag.add_assuming_missing dag node31 node41;
  Format.printf "%a@." dag_pp_mynode node;
  let name node = node.data.name in
  try
    Dag.add_assuming_missing dag node41 node;
    print_endline "no cycle"
  with
  | Dag.Cycle cycle ->
    let cycle = List.map cycle ~f:name in
    List.map ~f:Pp.text cycle |> Pp.concat ~sep:Pp.space |> print;
    [%expect
      {|
(1: k=1) (root) [(3: k=1) (child 1 2) [(4: k=1) (child 2 1) [(5: k=2) (child 3 1) [
                                                             ]]];
                  (2: k=1) (child 1 1) []]
(1: k=1) (root) [(3: k=1) (child 1 2) [(4: k=1) (child 2 1) [(5: k=2) (child 3 1) [
                                                             (6: k=2) (child 4 1) [
                                                             ]]]];
                  (2: k=1) (child 1 1) []]
child 4 1 child 3 1 child 2 1 child 1 2 root child 4
1
|}]

let rec adjacent_pairs l =
  match l with
  | []
  | [ _ ] ->
    []
  | x :: y :: rest -> (x, y) :: adjacent_pairs (y :: rest)

let cycle_test variant =
  let module Dag = DagF.Make (struct
    type t = int
  end) in
  let open Dag in
  let node dag data = { info = create_node_info dag; data } in
  let edges = ref [] in
  let add d n1 n2 =
    edges := (n1.data, n2.data) :: !edges;
    add_assuming_missing d n1 n2
  in
  let d = Dag.create () in
  let _n1 = node d 1 in
  let n2 = node d 2 in
  let n3 = node d 3 in
  (* the two variants are equivalent, but they end up taking a different code
     path when producing the cycle for some reason (or at least they did in
     2019-03) *)
  (match variant with
  | `a -> add d n2 n3
  | `b -> ());
  let n4 = node d 4 in
  add d n3 n4;
  let n5 = node d 5 in
  add d n5 n2;
  let n6 = node d 6 in
  add d n6 n3;
  let n7 = node d 7 in
  let n8 = node d 8 in
  add d n7 n8;
  let n9 = node d 9 in
  add d n8 n9;
  let n10 = node d 10 in
  add d n9 n10;
  let n11 = node d 11 in
  add d n10 n11;
  let n12 = node d 12 in
  add d n11 n12;
  let n13 = node d 13 in
  add d n12 n13;
  let n14 = node d 14 in
  add d n13 n14;
  let n15 = node d 15 in
  add d n14 n15;
  let n16 = node d 16 in
  add d n15 n16;
  let n17 = node d 17 in
  add d n16 n17;
  let n18 = node d 18 in
  add d n17 n18;
  let n19 = node d 19 in
  add d n12 n19;
  let n20 = node d 20 in
  add d n10 n20;
  let n21 = node d 21 in
  add d n20 n21;
  let n22 = node d 22 in
  add d n21 n22;
  let n23 = node d 23 in
  add d n22 n23;
  let n24 = node d 24 in
  add d n23 n24;
  let n25 = node d 25 in
  add d n24 n25;
  let n26 = node d 26 in
  add d n25 n26;
  let n27 = node d 27 in
  add d n26 n27;
  let n28 = node d 28 in
  add d n21 n28;
  let n29 = node d 29 in
  add d n10 n29;
  let n30 = node d 30 in
  add d n8 n30;
  let _n31 = node d 31 in
  add d n14 n20;
  match add d n23 n11 with
  | _ -> assert false
  | exception Cycle c ->
    let c = List.map c ~f:(fun x -> x.data) in
    List.iter (adjacent_pairs c) ~f:(fun (b, a) ->
        match List.exists !edges ~f:(fun edge -> edge = (a, b)) with
        | true -> ()
        | false -> Printf.ksprintf failwith "bad edge in cycle: (%d, %d)\n" a b);
    List.map c ~f:(Pp.textf "%d") |> Pp.concat ~sep:Pp.space |> print

let%expect_test _ =
  cycle_test `a;
  [%expect {|
23 22 21 20 14 13 12 11
23
|}]

let%expect_test _ =
  cycle_test `b;
  [%expect {|
23 22 21 20 14 13 12 11
23
|}]
