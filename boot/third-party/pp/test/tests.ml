open StdLabels
open Pp.O

let print pp = Format.printf "%a@." Pp.to_fmt pp

let many n pp = Array.make n pp |> Array.to_list |> Pp.concat ~sep:Pp.space

let xs n = many n (Pp.char 'x')

let ys n = many n (Pp.char 'y')

let%expect_test _ =
  let hello_xs n = Pp.text "Hello" ++ Pp.space ++ xs n in
  print (Pp.box ~indent:2 (hello_xs 200));
  [%expect
    {|
Hello x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x
|}];
  print (Pp.hbox (hello_xs 50));
  [%expect
    {|
Hello x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
|}];
  print (Pp.vbox ~indent:2 (hello_xs 5));
  [%expect {|
Hello
  x
  x
  x
  x
  x
|}];
  print (Pp.hvbox ~indent:2 (hello_xs 5));
  [%expect {|
Hello x x x x x
|}];
  print (Pp.hvbox ~indent:2 (hello_xs 50));
  [%expect
    {|
Hello
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
  x
|}];
  print (Pp.hovbox ~indent:2 (hello_xs 200));
  [%expect
    {|
Hello x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x
|}]

(* Difference between box and hovbox *)
let%expect_test _ =
  let pp f = f (xs 50 ++ Pp.break ~nspaces:2 ~shift:(-1) ++ xs 10) in
  print (pp (Pp.box ~indent:2));
  [%expect
    {|
x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x
 x x x x x x x x x x
|}];
  print (pp (Pp.hovbox ~indent:2));
  [%expect
    {|
x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x  x x x x x x x x x x
|}]

let enum_x_and_y = Pp.enumerate [ xs; ys ] ~f:(fun f -> f 50)

let%expect_test _ =
  print enum_x_and_y;
  [%expect
    {|
- x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
  x x x x x x x x x x x x
- y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y
  y y y y y y y y y y y y
|}]

let%expect_test _ =
  print
    (Pp.enumerate
       [ Pp.enumerate [ "abc"; "def" ] ~f:Pp.text; enum_x_and_y ]
       ~f:(fun x -> x));
  [%expect
    {|
- - abc
  - def
- - x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
    x x x x x x x x x x x x x
  - y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y y
    y y y y y y y y y y y y y
|}]

let%expect_test _ =
  print (Pp.verbatim "....." ++ Pp.box ~indent:2 (xs 50));
  [%expect
    {|
    .....x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x x
           x x x x x x x x x x x x x x |}]

let%expect_test _ =
  print
    (Pp.vbox
       ( Pp.box (Pp.text "Error: something went wrong!")
       ++ Pp.cut
       ++ Pp.box (Pp.text "Here are a few things you can do:")
       ++ Pp.cut
       ++ Pp.enumerate
            ~f:(fun x -> x)
            [ Pp.text
                "read the documentation, double check the way you are using \
                 this software to make sure you are not doing something wrong, \
                 and hopefully fix the problem on your side and move on"
            ; Pp.text
                "strace furiously the program to try and understand why \
                 exactly it is trying to do what it is doing"
            ; Pp.text "report an issue upstream"
            ; Pp.text "if all else fails"
              ++ Pp.cut
              ++ Pp.enumerate ~f:Pp.text
                   [ "scream loudly at your computer"
                   ; "take a break from your keyboard"
                   ; "clear your head and try again"
                   ]
            ] ));
  [%expect
    {|
    Error: something went wrong!
    Here are a few things you can do:
    - read the documentation, double check the way you are using this software to
      make sure you are not doing something wrong, and hopefully fix the problem
      on your side and move on
    - strace furiously the program to try and understand why exactly it is trying
      to do what it is doing
    - report an issue upstream
    - if all else fails
      - scream loudly at your computer
      - take a break from your keyboard
      - clear your head and try again |}]

(* Wrap the formatted lines *)
let%expect_test _ =
  print
    (Pp.hovbox ~indent:2
       ( Array.make 50 (Pp.char 'x')
       |> Array.to_list
       |> Pp.concat
            ~sep:(Pp.custom_break ~fits:("", 2, "") ~breaks:(" \\", -1, "")) ));
  [%expect
    {|
x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x \
 x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x  x
|}]
