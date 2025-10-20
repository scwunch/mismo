require "./spec_helper"
require "../src/zig/*"

module Mismo
  def self.run(file_path : String, line_offset, program : String)
    # tempfile = File.tempfile("zig_code", suffix: ".zig")
    main_path = __DIR__ + "/" + "../src/zig/main.zig"
    File.copy(File.join(__DIR__, "../src/zig/prelude.zig"), main_path)
    main_file = File.open(main_path, "a")
    main_file << '\n'
    code_generator = CodeGenerator.new(main_file)
    type_env = type_check_program(file_path, line_offset, program, :info)
    # code_generator.emit(type_env.functions["main"].first)
    code_generator.generate_program(type_env)
    main_file.close
    zig_build_run
  end
end

describe "Integration Testing" do
  it "should compile and run", focus: true do
    result = Mismo.run(
      file_path: __FILE__,
      line_offset: __LINE__ + 2, 
      program: <<-MISMO
      def main:
        let greeting = "Hello World"
        1234.string.print
        print(greeting)
      MISMO
    )
    result.should eq "1234\nHello World\n"
  end

  it "should handle basic arithmetic" do
    result = Mismo.run(
      file_path: __FILE__, 
      line_offset: __LINE__ + 2, 
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
