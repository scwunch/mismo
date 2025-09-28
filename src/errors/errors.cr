# base class for all errors emitted by the compiler
abstract struct Error
  property location : Location
  def initialize(@location : Location)
  end
  def to_s(io : IO)
    io << self.class << "@" << location
  end
  abstract def print(prefix : String, source_lines : Array(String), io : IO = STDOUT)
end

module ErrorWithMessage
  property message : String
  
  def initialize(@location : Location, @message : String)
  end
  
  def to_s(io : IO)
    super(io)
    io << ": " << message
  end

  def print(prefix : String, source_lines : Array(String), io : IO = STDOUT)
    io << prefix
    io << self.class << "@" << location << ": " << message
    io << '\n'
    if 0 < location.line && location.line < source_lines.size
      io << ' '
      io << prefix
      io << source_lines[location.line - 1]
      if location.column != 0
        io << "\n "
        io << prefix
        io << " " * (location.column - 1)
        io << "^\n"
      else
        io << '\n'
      end
    end
  end
end

# TODO: currently Parser::ParseError is used for syntax errors
struct SyntaxError < Error
  include ErrorWithMessage
end

abstract struct SemanticError < Error
end

struct MissingNameError < SemanticError
  include ErrorWithMessage
end

struct MissingOverloadError < SemanticError
  include ErrorWithMessage
end
struct AmbiguousFunctionCallError < SemanticError
  include ErrorWithMessage
end


abstract struct TypeError < SemanticError
end

struct TypeMismatchError < TypeError
  property expected : Type
  property actual : Type
  @annotation : Location
  def initialize(@location : Location, @expected : Type, @actual : Type, @annotation : Location)
  end
  def to_s(io : IO)
    super(io)
    io << ": expected " << expected << ", got " << actual << " (from " << @annotation << ")"
  end

  def print(prefix : String, source_lines : Array(String), io : IO = STDOUT)
    io << prefix
    io << self.class << "@" << location << ": expected " << expected << ", got " << actual
    io << '\n'
    if 0 < location.line && location.line < source_lines.size
      io << ' '
      io << prefix
      io << source_lines[location.line - 1]
      if location.column != 0
        io << "\n "
        io << prefix
        io << " " * (location.column - 1)
        io << "^\n"
      else
        io << '\n'
      end
    end
    if 0 < @annotation.line && @annotation.line < source_lines.size
      io << prefix << "Expected " << expected << " because of this line:\n"
      io << source_lines[@annotation.line - 1]
      if @annotation.column != 0
        io << "\n "
        io << prefix
        io << " " * (@annotation.column - 1)
        io << "^\n"
      else
        io << '\n'
      end
    end
  end
end

struct IllegalMutationError < TypeError
  include ErrorWithMessage
end

struct MetatypeError < TypeError
  include ErrorWithMessage
end

struct IllegalTargetError < SemanticError
  include ErrorWithMessage
end

struct LifetimeError < SemanticError
  include ErrorWithMessage
end

struct TraitError < SemanticError
  include ErrorWithMessage
end