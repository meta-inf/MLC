(* Setup test environment for toplevel. May need to run ocamlbuild to get 
 * related binaries first. *)

#directory "../_build";;
#directory "../_build/interp";;
#directory "../_build/parse";;
#directory "../_build/native";;

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
#load_rec "judge_reg.cmo";;
#load "unix.cma";;

#use "judge.ml";;
