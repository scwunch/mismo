# base class for all errors emitted by the compiler
abstract struct Error
  property location : Location
  def initialize(@location : Location)
  end
  def source
    location.source
  end
  def to_s(io : IO)
    io << self.class << "@" << location
  end
  abstract def print(prefix : String, io : IO = STDOUT)
end

module CanError(Ex)
  abstract def log : Logger

  def emit_error(error : Error)
    log.error(error)
    error
  end

  def abort!(error)
    emit_error(error)
    raise Ex.new
  end
end

module ErrorWithMessage
  property message : String
  
  def initialize(@location : Location, @message : String)
  end
  
  def to_s(io : IO)
    super(io)
    io << ": " << message
  end

  def print(prefix : String, io : IO = STDOUT)
    io << prefix
    io << self.class << " from " << location << ": " << message
    io << '\n'
    if 0 < location.line && location.line < source.size
      io << ' '
      io << prefix
      io << source[location.line]
      if location.column != 0
        io << "\n "
        io << prefix
        io << " " * location.indent
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
struct ArgumentMismatchError < SemanticError
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

  def print(prefix : String, io : IO = STDOUT)
    io << prefix
    io << self.class << " from " << location << ": expected " << expected << ", got " << actual
    io << '\n'
    if 0 < location.line && location.line < source.size
      io << ' '
      io << prefix
      io << source[location.line]
      if location.column != 0
        io << "\n "
        io << prefix
        io << " " * location.indent
        io << "^\n"
      else
        io << '\n'
      end
    end
    io << prefix << "Expected " << expected << " because of line " << @annotation << ":\n"
    if 0 < @annotation.line && @annotation.line < source.size
      io << source[@annotation.line]
      if @annotation.column != 0
        io << "\n "
        io << prefix
        io << " " * @annotation.indent
        io << "^\n"
      else
        io << '\n'
      end
    else
      io << "HEEY!  Where did my source_lines go?"
    end
  end
end

struct TypeSatisfiesConstraintsError < TypeError
  property type : Type
  property constraints : TypeParameter
  @annotation : Location
  def initialize(@location : Location, @type : Type, @constraints : TypeParameter, @annotation : Location)
  end
  def to_s(io : IO)
    super(io)
    io << ": " << type << " does not satisfy constraints " << constraints << " (from " << @annotation << ")"
  end
  def print(prefix : String, io : IO = STDOUT)
    io << prefix
    io << self.class << " " << location << " expected " << constraints << ", got " << type
    io << '\n'
    if 0 < location.line && location.line < source.size
      io << ' '
      io << prefix
      io << source[location.line]
      if location.column != 0
        io << "\n "
        io << prefix
        io << " " * location.indent
        io << "^\n"
      else
        io << '\n'
      end
    end
    if 0 < @annotation.line && @annotation.line < source.size
      io << prefix << "Constraints " << constraints << " from this line:\n"
      io << source[@annotation.line]
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