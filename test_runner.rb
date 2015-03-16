# test files are of the form
# comment
# #-#-#
# code
# #-#-#
# output
# this allows for testing of errors etc where the error message generated by ocaml is known
# comment is mandatory and gives an overview of the purpose of the test

require 'diffy'
require 'yaml'

errors = []

class Error
  attr_accessor :name, :diff

  def initialize(name, diff)
    @name = name
    @diff = diff
  end

  def inspect
    %(#{@name}\n\n#{@diff}\n\n)
  end
end


Dir["test/*_test.aa"].each do |file|
  test_data = File.read(file)
  frontmatter, code, expected_output = test_data.split('#-#-#').collect(&:strip)
  frontmatter = YAML.load(frontmatter)
  if code.nil?
    abort "#{file} appears to in an incorrect format"
  elsif frontmatter['comment'].nil?
    abort "#{file} missing comment"
  end
  File.open('./test/test.aa', 'w') { |f| f.write(code) }
  r, w = IO.pipe
  pid = spawn('./language test/test.aa', out: w, err: w)
  Process.wait pid
  w.close
  result = r.read.strip
  if result == expected_output
    print "."
  else
    errors << Error.new(frontmatter['comment'] ,Diffy::Diff.new(expected_output, result))
    print "F"
  end
end

File.delete './test/test.aa'

print "\n"

if errors.length > 0
  errors.map { |obj| puts obj.inspect }
  exit 1
end
