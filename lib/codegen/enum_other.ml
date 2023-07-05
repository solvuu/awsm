open! Core
open! Import

(* Enum_other provides a workaround for unknown enums encountered at runtime from API endpoints. *)
let to_string =
  Ast_helper.Exp.case
    (Ast_convenience.pconstr
       "Non_static_id"
       [ Ast_helper.Pat.var (Ast_convenience.mknoloc "s") ])
    (Ast_convenience.evar "s")
;;

let of_string ~loc =
  let other_e = [%expr Non_static_id x] in
  other_e
;;

let type_decl ~loc =
  Ast_helper.Type.constructor
    (Ast_convenience.mknoloc "Non_static_id")
    ~args:(Pcstr_tuple [ [%type: string] ])
;;
