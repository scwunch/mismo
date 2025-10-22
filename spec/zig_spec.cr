require "./spec_helper"
require "../src/zig/*"

pending CodeGenerator do
  it "should emit a function" do
    io = IO::Memory.new
    code_generator = CodeGenerator.new(io)
    type_env = type_check_program(
      __FILE__, 
      __LINE__ + 2, 
      <<-MISMO
      def main(a Int) Int:
        1
      MISMO
    )
    code_generator.emit(type_env.functions["main"].first, 0)
    io.to_s.should eq <<-ZIG
    fn main(a: i64) i64 {
      1;
    }

    ZIG
  end

  
end