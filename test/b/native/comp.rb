#! env ruby
s = ARGV[0]
fail if not system("./comp.d.byte < #{s} > tmp.asm")
fail if not system("yasm -g dwarf2 -f elf64 tmp.asm")
fail if not system("gcc -o tmp tmp.o heap.o runtime.o")

