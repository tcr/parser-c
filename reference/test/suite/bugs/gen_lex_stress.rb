#!/usr/bin/ruby
def indent(str,i)
  str.split("\n").map { |s| (" "*i) + s }.join("\n")
end
if_stmt=<<-EOF
if (0) 
{
  ;
}
EOF

($stderr.puts "Usage: ./gen_lex_stress.rb number-of-levels" ; exit 1) unless ARGV.first
levels = ARGV.first.to_i
if(levels > 5000) 
  $stderr.puts "Warning: Creating more than 5000 levels isn't recommended and maybe crash your system"
  exit 1
end
preamble=<<-EOF
/* Lexer stress test (#{levels} levels)
 * Produces nested if then else, with increasing indentation.
 * The lexer shouldn't consume too much memory (try +RTS -32M -RTS) or take too much time
 * gcc doesn't have any problems with this one, and only takes ~2.5s for 5K levels (240 Mb)
 */
void foo() 
{
EOF

puts preamble
i=4
1.upto(levels) do
  puts indent(if_stmt,i)
  puts indent("else",i)
  i+=4
end
puts indent(if_stmt,i)
puts "}"