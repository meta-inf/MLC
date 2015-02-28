(* Setup test environment for toplevel. May need to run ocamlbuild to get 
 * related binaries first. *)

#directory "../_build";;
#directory "../_build/interp";;
#directory "../_build/parse";;

#load_rec "ast.cmo";;
#load_rec "parse.cmo";;
#load_rec "value.cmo";;
#load_rec "interp.cmo";;
#load "unix.cma";;

