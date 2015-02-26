#! env ruby
msg = ARGV[0]
msg = "..." if not msg
abort if not system("git add --all . && git status")
print "Confirm? [Y/n] "
s = $stdin.gets.chomp
abort if s != "" and s[0] != 'Y'
abort if not system("git commit -m \"#{msg}\" && git push origin master")
