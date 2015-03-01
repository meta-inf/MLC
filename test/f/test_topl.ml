(* Setup test environment for toplevel. May need to run ocamlbuild to get 
 * related binaries first. *)

#directory "../../f/_build";;
#directory "../../f/_build/parse";;
#directory "../../f/_build/typec";;

#load_rec "ast.d.cmo";;
#load_rec "iAst.d.cmo";;
#load_rec "il.d.cmo";;
#load_rec "syntax.d.cmo";;
#load_rec "parser.d.cmo";;
#load_rec "types.d.cmo";;
#load_rec "pretype.d.cmo";;
#load_rec "env.d.cmo";;
#load_rec "test.d.cmo";;
#load_rec "typec.d.cmo";;
#load_rec "env.d.cmo";;
#load_rec "patcomp.d.cmo";;
#load "unix.cma";;

#use "test_main.ml";;


