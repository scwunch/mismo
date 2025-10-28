require "./spec_helper"
require "../src/zig/*"

module Mismo
  def self.run(file_path : String, starting_line, program : String)
    # tempfile = File.tempfile("zig_code", suffix: ".zig")
    main_path = __DIR__ + "/" + "../src/zig/main.zig"
    ::File.copy(::File.join(__DIR__, "../src/zig/prelude.zig"), main_path)
    main_file = ::File.open(main_path, "a")
    main_file << '\n'
    code_generator = CodeGenerator.new(main_file)
    type_env = type_check_program(file_path, starting_line, program, :debug)
    unless type_env.log.has_errors?
      code_generator.generate_program(type_env)
      main_file.close
      zig_build_run
    else
      p! type_env.log.@out
    end
  end
end

describe "Integration Testing" do
  it "should compile and run" do #, focus: true do
    result = Mismo.run(
      file_path: __FILE__,
      starting_line: __LINE__ + 2, 
      program: <<-MISMO
      def main:
        let greeting = "Hello World"
        1234.string.print
        print(greeting)
      MISMO
    )
    result.should eq "1234\nHello World\n"
  end

  pending "should handle basic arithmetic" do
    result = Mismo.run(
      file_path: __FILE__, 
      starting_line: __LINE__ + 2, 
      program: <<-MISMO
      def main:
        let answer = 1 + 2
        answer.string.print
      MISMO
    )
    p! result
    result.should eq "3"
  end
end
