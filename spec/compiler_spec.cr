require "./spec_helper"
require "../src/compiler"

describe Compiler do
  it "should compile and run" do
    program = <<-MISMO
      def main:
        let greeting = "Hello World"
        1234.string.print
        print(greeting)
      MISMO
    result = Compiler.new(
      target_stage: Compiler::Stage::Run,
      log_level: Logger::Level::Debug,
      # prelude_path: nil,
    ).add_file(
      file_path: __FILE__,
      starting_line: __LINE__ - 10, 
      input: program,
      log_level: Logger::Level::Debug,
    ).run
    result.should eq "1234\nHello World\n"
  end
end