(* TODO: Is this module needed. We adapted this from ppx_tools_versioned's
   Ast_convenience_404 in support of porting our code from ppx_tools_versioned to
   ppxlib. Does ppxlib already provide all of this? Also, is all of this being used? *)
open! Core
open! Import

module Label = struct
  type t = Asttypes.arg_label

  type desc = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  let explode x = x
  let nolabel = Nolabel
  let labelled x = Labelled x
  let optional x = Optional x
end

let mkloc = Ocaml_common.Location.mkloc
let mknoloc = Ocaml_common.Location.mknoloc
let default_loc = Ast_helper.default_loc
let lid ?(loc = !default_loc) s = mkloc (Longident.parse s) loc
let evar ?loc ?attrs s = Ast_helper.Exp.ident ?loc ?attrs (lid ?loc s)

let may_tuple ?loc tup = function
  | [] -> None
  | [ x ] -> Some x
  | l -> Some (tup ?loc ?attrs:None l)
;;

let pconstr ?loc ?attrs s args =
  Ast_helper.Pat.construct
    ?loc
    ?attrs
    (lid ?loc s)
    (may_tuple ?loc Ast_helper.Pat.tuple args)
;;

let constr ?loc ?attrs s args =
  Ast_helper.Exp.construct
    ?loc
    ?attrs
    (lid ?loc s)
    (may_tuple ?loc Ast_helper.Exp.tuple args)
;;

let pvar ?(loc = !default_loc) ?attrs s = Ast_helper.Pat.var ~loc ?attrs (mkloc s loc)

let str ?(loc = !default_loc) ?attrs s =
  Ast_helper.Exp.constant ?attrs (Pconst_string (s, loc, None))
;;

let unit ?loc ?attrs () = constr ?loc ?attrs "()" []

let tuple ?loc ?attrs = function
  | [] -> unit ?loc ?attrs ()
  | [ x ] -> x
  | xs -> Ast_helper.Exp.tuple ?loc ?attrs xs
;;

let ptuple = Ast_helper.Pat.tuple
let nil ?loc ?attrs () = constr ?loc ?attrs "[]" []
let cons ?loc ?attrs hd tl = constr ?loc ?attrs "::" [ hd; tl ]
let some x = constr "Some" [ x ]
let pair x y = tuple [ x; y ]
let singleton x = cons x (nil ())

let int ?loc ?attrs x =
  Ast_helper.Exp.constant ?loc ?attrs (Pconst_integer (string_of_int x, None))
;;

let float ?loc ?attrs x =
  Ast_helper.Exp.constant ?loc ?attrs (Pconst_integer (string_of_float x, None))
;;

let app ?loc ?attrs f l =
  match l with
  | [] -> f
  | _ -> Ast_helper.Exp.apply ?loc ?attrs f (List.map l ~f:(fun a -> Label.nolabel, a))
;;

let app_labels ?loc ?attrs f l =
  match l with
  | [] -> f
  | _ -> Ast_helper.Exp.apply ?loc ?attrs f l
;;

let list ?loc ?attrs l =
  List.fold_right l ~init:(nil ?loc ?attrs ()) ~f:(cons ?loc ?attrs)
;;

let int32 ?loc ?attrs x =
  Ast_helper.Exp.constant ?loc ?attrs (Pconst_integer (Int32.to_string x, Some 'l'))
;;

let int64 ?loc ?attrs x =
  Ast_helper.Exp.constant ?loc ?attrs (Pconst_integer (Int64.to_string x, Some 'L'))
;;

let record ?loc ?attrs ?over l =
  Ast_helper.Exp.record
    ?loc
    ?attrs
    (List.map l ~f:(fun (s, e) -> lid ~loc:e.pexp_loc s, e))
    over
;;

let lam ?loc ?attrs ?(label = Label.nolabel) ?default pat exp =
  Ast_helper.Exp.fun_ ?loc ?attrs label default pat exp
;;

let pstr ?(loc = !Ast_helper.default_loc) ?attrs s =
  Ast_helper.Pat.constant ?attrs (Pconst_string (s, loc, None))
;;

let variant = Ast_helper.Exp.variant
let let_ = Ast_helper.Exp.let_

let http_method ?(loc = !Ast_helper.default_loc) meth =
  match meth with
  | `GET -> [%expr `GET]
  | `POST -> [%expr `POST]
  | `PUT -> [%expr `PUT]
  | `DELETE -> [%expr `DELETE]
  | `HEAD -> [%expr `HEAD]
  | `PATCH -> [%expr `PATCH]
;;

let structure_items_to_module_structure ?(loc = !Ast_helper.default_loc) name l =
  (* In ppx_tools_versioned '[%%s ...]' would take a list of structure items.
       This was replaced by '[%%i ...]' in ppxlib, which only takes a single
       structure item. So, we do this transformation to turn it into a
       module, then include it. *)
  Ast_helper.Str.module_
    (Ast_helper.Mb.mk (mkloc (Some name) loc) (Ast_helper.Mod.structure l))
;;
