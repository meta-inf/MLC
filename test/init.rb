#! env ruby
fail if not system("cd ../f && ocamlbuild main.d.byte && cp main.d.byte ../test/fe.d.byte")
fail if not system("cd ../b && ocamlbuild -r comp.d.byte && cp comp.d.byte ../test/comp.d.byte")
fail if not system("cd ../b && ocamlbuild -r -lib unix interpret.d.byte && cp interpret.d.byte ../test/interpret.d.byte")
fail if not system("gcc -c ../b/native/heap.c && gcc -c ../b/native/runtime.c")
