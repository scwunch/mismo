require "./spec_helper"
require "../src/interpreter"

def loc
  Location.zero
end

pending Interpreter do
  describe "#eval" do
    it "works" do
      interpreter = Interpreter.new(Logger.new)
      interpreter.eval(Hir.nil(loc)).should eq(Val.nil)
    end
  end

  describe "#run" do
    it "works" do
      program = <<-MISMO
        def main -> Int:
          var x = 45
          var y = 78
          x + y
          var rat = Rational(1, 2.abs)
          print(rat.float)
          rat = rat + 2
          float(rat * 10).round

        struct Rational
          var numerator Int
          var denominator Nat

          def float Float:
            .numerator.float / .denominator.float

          def +(other Int) Rational:
            Rational(.numerator + other * .denominator, .denominator)

          def -(other Int) Rational:
            Rational(.numerator - other * .denominator, .denominator)

          def *(other Int) Rational:
            Rational(.numerator * other, .denominator)

          def /(other Nat) Rational:
            Rational(.numerator, .denominator * other)
        MISMO
      Interpreter.run(program, file_path: __FILE__, line_offset: __LINE__).should eq(Val.new(25))
    end

    it "pattern matching" do
      program = <<-MISMO
        enum Color
          Red
          Green
          Blue

        def main String:
          let r = Color.Blue
          if r is 
            Green:
              "gren"
            Red:
              "Red!"
            _: 
              "some other color"
        MISMO
      Interpreter.run(program, file_path: __FILE__, line_offset: __LINE__).should eq(Val.new("some other color"))
    end
  end
end