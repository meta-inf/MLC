#! env ruby
s = ARGV[0]
t = s.sub(/\..+$/, '')
fail if not system("./fe.d.byte < #{s} | ./interpret.d.byte ")
