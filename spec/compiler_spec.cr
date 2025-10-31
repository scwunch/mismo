require "./spec_helper"
require "../src/compiler"

describe Compiler do
  it "should compile and run" do
    program = <<-MISMO
      def main:
        let greeting = "Hello World"
        1234.string.print
        print(greeting)
        let unused = "I'm not used anywehere"
        let a = 0
        var b = 1
        if a < 
          0:
            a.string.print
          -10:
            print("will not print")
        else:
          b = b + 1
          b.string.print

        let bool = true
        let nat = 1.nat
        let int = -2
        let float = 3.14

        string(bool + nat).print
        string(nat - int).print
        string(int / bool).print
        string(nat / nat).print
        string(nat / int).print
        string(int / nat).print
        string(int / int).print
        string(float / float).print
        
        
        

        
      MISMO
    result = Compiler.new(
      target_stage: Compiler::Stage::Run,
      log_level: Logger::Level::Info,
      # prelude_path: nil,
    ).add_file(
      file_path: __FILE__,
      starting_line: 7, 
      input: program,
      log_level: Logger::Level::Debug,
    ).run
    result.should eq "1234\nHello World\n1\n"
  end
end