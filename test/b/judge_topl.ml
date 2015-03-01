(* Setup test environment for toplevel. May need to run ocamlbuild to get 
 * related binaries first. *)

#directory "../../b/_build";;
#directory "../../b/_build/interp";;
#directory "../../b/_build/parse";;
#directory "../../b/_build/native";;

#load_rec "ast.cmo";;
#load_rec "parse.cmo";;
#load_rec "value.cmo";;
#load_rec "interp.cmo";;
#load_rec "native.cmo";;
#load_rec "cps.cmo";;
#load_rec "register.cmo";;
#load_rec "mgen.cmo";;
#load_rec "prereg.cmo";;
#load_rec "prep.cmo";;
#load_rec "test.cmo";;
#load "unix.cma";;

#use "judge.ml";;
