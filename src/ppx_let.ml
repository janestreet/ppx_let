open Ppxlib

let ext t ~locality extension_kind =
  Extension.declare_with_path_arg
    (* Prefix with the name of the ppx to help avoid clashes with other ppxes. *)
    ("let." ^ Ppx_let_expander.ext_full_name t ~locality extension_kind)
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc:_ ~path:_ ~arg expr ->
      Ppx_let_expander.expand t extension_kind ~modul:arg ~locality expr)
;;

open Ppx_let_expander

module List = struct
  include List

  let concat_map list ~f = List.concat_map f list
  let map list ~f = List.map f list
end

let () =
  let extensions =
    List.concat_map [ bind; map ] ~f:(fun t ->
      List.concat_map Ppx_let_expander.Locality.all ~f:(fun locality ->
        List.map
          Extension_kind.[ default; default_open; n; n_open; z; z_open; nz; nz_open ]
          ~f:(fun kind -> ext t ~locality kind)))
  in
  Driver.register_transformation "let" ~extensions
;;
