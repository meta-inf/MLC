#! env ruby

if (ARGV.length >= 1) and (ARGV[0] = "clear") then
	system("rm *.asm *.il *.e")
	exit
end

s = `ls | grep ".ml$"`.lines
failcnt = 0
s.each { |s|
	s.chomp!
	print "Running test #{"%-20s" % ("%s:" % s)} "
	so = s.gsub(/ml$/, 'exp')
	se = s.gsub(/ml$/, 'e')
	if not system("ruby comp.rb #{s}") then
		puts "%30s" % "Compile Error"
		failcnt += 1
	else
		if not system("./#{se} > out && diff -w -q out #{so}") then
			puts "%30s" % "FAILED"
			failcnt += 1
		else
			puts "%30s" % "Passed"
		end
	end
}

system("rm out")
