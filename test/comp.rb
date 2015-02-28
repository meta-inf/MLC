#! env ruby
s = ARGV[0]
t = s.sub(/\..+$/, '')
fail if not system("./fe.d.byte < #{s} > #{t}.il")
fail if not system("./comp.d.byte < #{t}.il > #{t}.asm")
fail if not system("yasm -g dwarf2 -f elf64 #{t}.asm")
fail if not system("gcc -o #{t}.e #{t}.o -g heap.o runtime.o && rm #{t}.o")

