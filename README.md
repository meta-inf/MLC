# MLC

Toy compiler for a subset of Caml.

Under construction.

## What's done

* Tuple, list, algebraic data types
* Pattern match (except for value declaration)

Check `test/` for details.

## TODO

* Decurrying
* Beta expansion
* Runtime support for string & array
* Readable error messages
* Recursive type inference (`-rectypes` in OCaml)

## Syntax

The syntax is slightly different from OCaml. Check `test/` and `f/parse/` for details.
