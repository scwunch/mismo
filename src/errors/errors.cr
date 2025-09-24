# base class for all errors emitted by the compiler
abstract struct Error
  property location : Location
  def initialize(@location : Location)
  end
  def to_s(io : IO)
    io << self.class << "@" << location
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
end

# TODO: currently Parser::ParseError is used for syntax errors
struct SyntaxError < Error
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