require "./hir_nodes"
require "./cell"

abstract struct Type

  abstract def to_s(io : IO)
  abstract def mode : Mode
  def primitive? : Bool; false end

  macro primitive
    private def initialize; end
    def self.instance ; @@instance ||= new end
    def Type.{{@type.name[6..].downcase}} ; {{@type.name[6..]}}.instance end
    def to_s(io : IO)
      io << "{{@type.name[6..]}}"
    end
    def mode : Mode ; Mode::Move end
    def primitive? : Bool; true end
  end

  struct Never < Type
    primitive
  end

  struct Nil < Type
    primitive
  end

  struct Bool < Type
    primitive
  end

  struct Int < Type
    primitive
  end

  struct Float < Type
    primitive
  end

  struct String < Type
    primitive
    def mode : Mode ; Mode::Ref end
  end

  struct Var < Type
    getter id : Int32
  
    def initialize(@id)
    end
  
    def to_s(io : IO)
      io << "T#{@id}"
    end

    def mode : Mode ; Mode::Let end
  end
  
  struct Array < Type
    getter element_type : Cell(Type)
    def initialize(@element_type : Cell(Type))
    end
    def initialize(element_type : Type)
      @element_type = Cell.new(element_type)
    end
    def Type.array(type) ; Array.new(type) end
    def to_s(io : IO)
      io << "Array[#{@element_type}]"
    end
    def mode : Mode ; Mode::Let end
  end

  struct Tuple < Type
    getter types : ::Array(Type)
    def initialize(@types : ::Array(Type))
    end
    def Type.tuple(*args) ; Tuple.new(*args) end
    def to_s(io : IO)
      io << "Tuple[#{types.join(", ")}]"
    end
    def mode : Mode ; Mode::Let end
  end

  struct Struct < Type
    getter base : StructBase
    getter type_args : ::Array(Type)
    def initialize(@base : StructBase, @type_args : ::Array(Type))
    end
    def Type.struct(*args) ; Struct.new(*args) end
    def to_s(io : IO)
      io << "struct #{@base.name}"
      io << "[#{type_args.join(", ")}]" if type_args.any?
    end
    def mode : Mode ; base.mode end
  end

  struct Enum < Type
    getter base : EnumBase
    getter type_args : ::Array(Type)
    def initialize(@base : EnumBase, @type_args : ::Array(Type))
    end
    def Type.enum(*args) ; Enum.new(*args) end
    def to_s(io : IO)
      io << "enum #{@base.name}"
      io << "[#{type_args.join(", ")}]" if type_args.any?
    end
    def mode : Mode ; base.mode end
  end

  struct Function < Type
    getter args : ::Array(Type)
    getter return_type : Cell(Type)
    def initialize(@args, @return_type)
    end
    def initialize(@args, return_type : Type)
      @return_type = Cell.new(return_type.as(Type))
    end
    def Type.function(*args) ; Function.new(*args) end
    def to_s(io : IO)
      io << "(#{args.join(", ")}) -> #{@return_type}"
    end
    def mode : Mode; Mode::Let end
  end
end


# --- Type Information ---

abstract class TypeInfo
  getter location : Location
  getter mode : Mode
  getter name : String
  getter type_params : Array(Ast::TypeParameter)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Array(Ast::TypeParameter) = [] of Ast::TypeParameter)
  end
end

class StructBase < TypeInfo
  getter fields : ::Array(Field)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Array(Ast::TypeParameter) = [] of Ast::TypeParameter, @fields : ::Array(Field) = [] of Field)
  end
end

class EnumBase < TypeInfo
  getter variants : ::Array(Variant)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Array(Ast::TypeParameter) = [] of Ast::TypeParameter, @variants : ::Array(Variant) = [] of Variant)
  end
end

struct Field
  getter location : Location
  getter binding : Mode
  getter name : String
  getter type : Type
  def initialize(@location : Location, @binding : Mode, @name : String, @type : Type)
  end
end

struct Variant
  getter location : Location
  getter name : String
  getter fields : ::Array(Field)
  def initialize(@location : Location, @name : String, @fields : ::Array(Field))
  end
end

class TraitBase < TypeInfo
  getter methods : ::Array(Ast::AbstractMethod)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Array(Ast::TypeParameter) = [] of Ast::TypeParameter, @methods : ::Array(Ast::AbstractMethod) = [] of Ast::AbstractMethod)
  end
end

# struct Method
#   getter location : Location
#   getter name : String
#   getter type_params : ::Array(Ast::TypeParameter)
#   getter args : ::Array(Parameter)
#   getter return_type : Type
#   def initialize(@location : Location, @name : String, @type_params : ::Array(Ast::TypeParameter), @args : ::Array(Parameter), @return_type : Type)
#   end
# end

class FunctionBase
  getter location : Location
  getter name : String
  getter type_params : ::Array(Ast::TypeParameter)
  property parameters : ::Array(Parameter) = [] of Parameter
  property return_type : Type = Type.nil
  property body : ::Array(Hir) = [] of Hir
  def initialize(@location : Location, @name : String, @type_params : ::Array(Ast::TypeParameter), @parameters : ::Array(Parameter) = [] of Parameter, @return_type : Type = Type.nil)
  end
end

struct Parameter
  getter location : Location
  getter mode : Mode
  getter name : String
  getter type : Type
  def initialize(@location : Location, @mode : Mode, @name : String, @type : Type)
  end
end

struct Trait
  getter base : TraitBase
  getter type_args : ::Array(Type)
  def initialize(@base : TraitBase, @type_args : ::Array(Type))
  end
  def Type.trait(*args) ; Trait.new(*args) end
  def to_s(io : IO)
    io << "trait #{@base.name}"
    io << "[#{type_args.join(", ")}]" if type_args.any?
  end
end