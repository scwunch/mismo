
# the compiler manages the entire compilation process
class Compiler
  property input : IO::Memory | IO::Buffered

  def initialize(@input)
  end
  def initialize(code : String)
    @input = IO::Memory.new(code)
  end

  # compile and run a Mismo program
  def run(io : IO)
    
  end
  def run(code : String)
    run(IO::Memory.new(code))
  end
  def run(path : String)
    run(File.open(path))
  end

  def lex(io : IO)
    Lexer.new(io).lex
  end
end