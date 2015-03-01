#! env ruby

def compile (s)
	return false if not system("./comp.d.byte < #{s} > tmp.asm")
	return false if not system("yasm -g dwarf2 -f elf64 tmp.asm")
	return false if not system("gcc -o tmp tmp.o heap.o runtime.o")
	return true
end

if (ARGV.length >= 1) and (ARGV[0] = "clear") then
	system("rm *.o")
	system("rm tmp tmp.asm comp.d.byte")
	exit
end

system("cp ../../../b/comp.d.byte ./comp.d.byte && gcc -g -c ../../../b/native/runtime.c -DDEBUG1 && gcc -g -c ../../../b/native/heap.c -DDEBUG")
s = `ls | grep ".ilt$"`.lines
failcnt = 0
s.each { |s|
	s.chomp!
	print "Running test #{"%-20s" % ("%s:" % s)} "
	so = s.gsub(/ilt$/, 'out')
	if not compile(s) then
		puts "%30s" % "Compile Error"
		failcnt += 1
	else
		if not system("./tmp > out && diff -w -q out #{so}") then
			puts "%30s" % "FAILED"
			failcnt += 1
		else
			puts "%30s" % "Passed"
		end
	end
}


