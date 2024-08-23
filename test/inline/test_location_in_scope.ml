open! Core
open Ppxlib

let test (with_location_actual : Ppx_let_expander.With_location.t) =
  (let loc = Location.in_file "i-will-appear-in-the-location-of-the-callsite-setting" in
   Ppx_let_expander.expand
     (module struct
       include (val Ppx_let_expander.map)

       let with_location = with_location_actual
     end)
     Ppx_let_expander.Extension_kind.default
     ~locality:Locality.global
     ~modul:None
     [%expr
       let PAT = EXPR in
       BODY])
  |> Pprintast.string_of_expression
  |> print_endline
;;

let%expect_test "No location" =
  test No_location;
  [%expect {| Let_syntax.map EXPR ~f:(fun (PAT) -> BODY) |}]
;;

let%expect_test "Location of callsite" =
  test Location_of_callsite;
  [%expect
    {|
    Let_syntax.map
      ~here:{
              Ppx_here_lib.pos_fname =
                "i-will-appear-in-the-location-of-the-callsite-setting";
              pos_lnum = 1;
              pos_cnum = (-1);
              pos_bol = 0
            } EXPR ~f:(fun (PAT) -> BODY)
    |}]
;;

let%expect_test "Location in scope" =
  test (Location_in_scope "over_there");
  [%expect {| Let_syntax.map ~here:over_there EXPR ~f:(fun (PAT) -> BODY) |}]
;;
