require "./types"
require "./hir_nodes"

# This module is included by TypeDefinition, TratiDef, and FunctionDef
module LocNameTypeParams
  getter location : Location
  getter name : String
  property type_params : ::Slice(TypeParameter)
  def initialize(@location : Location, @name : String, @type_params : ::Slice(TypeParameter) = ::Slice(TypeParameter).empty)
  end
end

# --- Type Information ---

struct TypeParameter < IrNode
  getter location : Location
  getter name : String
  getter required_traits : ::Slice(Trait)
  getter excluded_traits : ::Slice(Trait)
  def initialize(@location : Location, @name : String, @required_traits = ::Slice(Trait).empty, @excluded_traits = ::Slice(Trait).empty)
  end
  def to_s(io : IO)
    io << name
    if req = required_traits
      io << ": #{req.join(" & ")}" if req.any?
    end
    if exc = excluded_traits
      io << " ~#{exc.join(" ~")}" if exc.any?
    end
  end
end

abstract class TypeDefinition
  include LocNameTypeParams
  getter mode : Mode
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : ::Slice(TypeParameter) = ::Slice(TypeParameter).empty)
  end
end

class StructDef < TypeDefinition
  getter fields : ::Array(Field)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : ::Slice(TypeParameter) = ::Slice(TypeParameter).empty, @fields : ::Array(Field) = [] of Field)
  end
  def to_s(io : IO)
    io << "struct #{@name}"
    io << "[#{@type_params.join(", ")}]" if @type_params.any?
  end
  def inspect(io : IO)
    io << "struct #{@name}"
    io << "[#{@type_params.join(", ")}]" if @type_params.any?
    io << "(#{fields.join(", ")})" if fields.any?
  end
end

class EnumDef < TypeDefinition
  getter variants : ::Array(Variant)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : ::Slice(TypeParameter) = ::Slice(TypeParameter).empty, @variants : ::Array(Variant) = [] of Variant)
  end
  def to_s(io : IO)
    io << "enum #{@name}"
    io << "[#{@type_params.join(", ")}]" if @type_params.any?
  end
  def inspect(io : IO)
    io << "enum #{@name}"
    io << "[#{@type_params.join(", ")}]" if @type_params.any?
    io << "(#{@variants.join(", ")})" if @variants.any?
  end
end

struct Field < IrNode
  getter location : Location
  getter binding : Binding
  getter name : String
  getter type : Type
  def initialize(@location : Location, @binding : Binding, @name : String, @type : Type)
  end
  def to_s(io : IO)
    io << "#{binding} #{name}: #{type}"
  end
end

struct Variant < IrNode
  getter location : Location
  getter name : String
  getter fields : ::Array(Field)
  def initialize(@location : Location, @name : String, @fields : ::Array(Field))
  end
  def to_s(io : IO)
    io << @name
    io << "(#{fields.join(", ")})" if fields.any?
  end
end

class TraitDef
  include LocNameTypeParams
  getter mode : Mode
  getter methods : ::Array(Ast::AbstractMethod)
  def initialize(@location : Location, @mode : Mode, @name : String, type_params : ::Slice(TypeParameter)? = nil, @methods : ::Array(Ast::AbstractMethod) = [] of Ast::AbstractMethod)
    @type_params = type_params || Slice[TypeParameter.new(@location, "Self")]
  end
  def to_s(io : IO)
    io << "trait #{@name}"
    io << "[#{type_params.join(", ")}]" if type_params.any?
  end
  def inspect(io : IO)
    io << "trait #{@name}"
    io << "[#{type_params.join(", ")}]" if type_params.any?
    io << "(#{methods.join(", ")})" if methods.any?
  end
end

# struct Method
#   getter location : Location
#   getter name : String
#   getter type_params : ::Slice(TypeParameter)
#   getter args : ::Array(Parameter)
#   getter return_type : Type
#   def initialize(@location : Location, @name : String, @type_params : ::Slice(TypeParameter), @args : ::Array(Parameter), @return_type : Type)
#   end
# end

class FunctionDef
  include LocNameTypeParams
  property parameters : ::Array(Parameter) = [] of Parameter
  property return_mode : Mode = Mode::Move
  property return_type : Type = Type.nil
  property body : ::Array(Hir) = [] of Hir
  @external_implementation : Bool = false
  def body=(body : ::Array(Hir)) ; @body = body ; end
  def body=(body : Hir) 
    case body
    when Hir::Block
      @body = body.statements
    else
      @body = [body]
    end
  end
  def initialize(@location : Location, @name : String, @type_params : ::Slice(TypeParameter) = ::Slice(TypeParameter).empty, @parameters : ::Array(Parameter) = [] of Parameter, @return_type : Type = Type.nil, @body = [] of Hir)
  end

  def to_s(io : IO)
    io << "def "
    io << name
    io << "[#{type_params.join(", ")}]" if type_params.any?
    io << "(#{parameters.join(", ")})"
    unless return_type == Type.nil
      io << " -> "
      io << return_mode << ' ' unless return_mode == Mode::Move
      io << return_type
    end
  end

  def extern? ; @external_implementation end
  def set_as_extern
    @external_implementation = true
  end
end

struct Parameter < IrNode
  getter location : Location
  getter mode : Mode
  getter name : String
  getter type : Type
  def initialize(@location : Location, @mode : Mode, @name : String, @type : Type)
  end
  def to_s(io : IO)
    io << mode << ' ' if mode
    io << name
    io << ": "
    io << type
  end
end

struct Trait
  getter base : TraitDef
  getter type_args : ::Slice(Type)
  def initialize(@base : TraitDef, @type_args : ::Slice(Type) = ::Slice(Type).empty)
    if @base.type_params.size != @type_args.size
      raise "wrong number of type args: #{inspect}"
    end
  end
  # def self.unknown(name : String, type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   Trait
  #     .new(TratiDef.new(Location.zero, Mode::Let, name))
  #     .with_type_args(type_args)
  # end
  def initialize(@base, type_arg : Type)
    @type_args = Slice[type_arg]
    if @base.type_params.size != 1
      p! @base
      p! @base.type_params
      p! @type_args
      raise "wrong number of type args: #{inspect}"
    end
  end
  def Type.trait(*args) ; Trait.new(*args).as Trait end
  def to_s(io : IO)
    io << "trait #{@base.name}"
    io << "[#{type_args.join(", ")}]" if type_args.any?
  end
  def mode : Mode ; base.mode end
  # def substitute_Self_with(type : Type) : Trait
  #   Trait.new(@base, @type_args.map { |ta| ta.substitute_Self_with(type) })
  # end
  def substitute_Self_with(type : Type) : Trait
    type_args = @type_args.dup
    type_args[0] = type
    Trait.new(@base, type_args)
  end
  # def with_type_args(type_args : ::Slice(Type)) : Trait
  #   @type_args = type_args
  #   self
  # end
  def inspect(io : IO)
    io << "Trait(#{@base.name}"
    io << "[#{type_args.join(", ")}]" if type_args.any?
    io << ")"
  end
end