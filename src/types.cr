require "./hir_nodes"
require "./cell"
require "./abstract_node"

abstract struct Type

  abstract def to_s(io : IO)
  abstract def mode : Mode
  def primitive? : Bool; false end

  def inspect(io : IO)
    io << {{@type.name.stringify}} << '('
    {% for ivar, i in @type.instance_vars %}
      io << {{ivar.name.id}}.inspect
      {% if i < @type.instance_vars.size - 1 %}
        io << ", "
      {% end %}
    {% end %}
    io << ')'
  end

  # abstract def substitute_Self_with(type : Type) : Type
    

  macro primitive
    private def initialize; end
    def self.instance ; @@instance ||= new end
    def Type.{{@type.name[6..].downcase}} : Type ; {{@type.name[6..]}}.instance.as Type end
    def to_s(io : IO)
      io << "{{@type.name[6..]}}"
    end
    def mode : Mode ; Mode::Move end
    def primitive? : Bool; true end
    def ==(other : Type)
      self.class == other.class
    end
    # def substitute_Self_with(type : Type) : Type
    #   self
    # end
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

  struct Nat < Type
    primitive
  end

  struct Float < Type
    primitive
  end

  struct String < Type
    primitive
    def mode : Mode ; Mode::Ref end
  end
  
  # struct Self < Type
  #   primitive
  #   def mode : Mode ; Mode::Let end
  #   def substitute_Self_with(type : Type) : Type
  #     type
  #   end
  # end

  struct Var < Type
    getter id : Int32
  
    def initialize(@id)
      # raise "I found the T1!" if @id == 1
    end
    def Type.var(*args) ; Var.new(*args).as Type end
  
    def to_s(io : IO)
      io << "T#{@id}"
    end

    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   self
    # end
  end

  struct Unknown < Type
    getter name : ::String
    getter type_args : Slice(Type)
    def initialize(@name : ::String, @type_args : Slice(Type) = Slice(Type).empty)
    end
    def Type.unknown(name, type_args = Slice(Type).empty)
      Unknown.new(name, type_args).as Type
    end
    def to_s(io : IO)
      io << "Unknown[#{@name}, #{@type_args.join(", ")}]"
    end
    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Type.unknown(@name, @type_args.map { |ta| ta.substitute_Self_with(type) })
    # end
  end

  struct Array < Type
    getter element_type : Cell(Type)
    def initialize(@element_type : Cell(Type))
    end
    def initialize(element_type : Type)
      @element_type = Cell.new(element_type.as(Type))
    end
    def Type.array(type) ; Array.new(type.as(Type)).as(Type) end
    def to_s(io : IO)
      io << "Array[#{@element_type}]"
    end
    def to_s
      "Array[#{@element_type}]"
    end
    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Array.new(@element_type.value.substitute_Self_with(type))
    # end
  end

  struct Tuple < Type
    getter types : Slice(Type)
    def initialize(@types : Slice(Type))
    end
    def initialize(types : ::Array(Type))
      @types = types.to_unsafe_slice
    end
    def Type.tuple(*args) ; Tuple.new(*args).as Type end
    def to_s(io : IO)
      io << "Tuple[#{@types.join(", ")}]"
    end
    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Tuple.new(@types.map { |t| t.substitute_Self_with(type) })
    # end
  end

  def Type.adt(base : StructBase, type_args : Slice(Type) = Slice(Type).empty)
    Struct.new(base, type_args).as Type
  end

  def Type.adt(base : EnumBase, type_args : Slice(Type) = Slice(Type).empty)
    Enum.new(base, type_args).as Type
  end

  def Type.adt(base : TypeInfo, type_args : Slice(Type) = Slice(Type).empty)
    case base
    when StructBase then Struct.new(base, type_args)
    when EnumBase then Enum.new(base, type_args)
    else
      raise "Type.adt: unknown type info: #{base}"
    end.as Type
  end

  struct Struct < Type
    getter base : StructBase
    getter type_args : Slice(Type)
    def initialize(@base : StructBase, @type_args : Slice(Type) = Slice(Type).empty)
    end
    def Type.struct(*args) ; Struct.new(*args).as Type end
    def to_s(io : IO)
      io << "struct #{@base.name}"
      io << "[#{@type_args.join(", ")}]" if @type_args.any?
    end
    def inspect(io : IO)
      io << "Type::Struct("
      base.inspect(io)
      io << "[#{@type_args.join(", ")}]" if @type_args.any?
      io << ")"
    end
    def mode : Mode ; base.mode end
    # def substitute_Self_with(type : Type) : Type
    #   Struct.new(@base, @type_args.map { |ta| ta.substitute_Self_with(type) })
    # end
    def get_field?(field_name : ::String) : Field?
      base.fields.each do |field|
        return field if field.name == field_name
      end
      nil
    end
  end

  struct Enum < Type
    getter base : EnumBase
    getter type_args : Slice(Type)
    def initialize(@base : EnumBase, @type_args : Slice(Type) = Slice(Type).empty)
    end
    def Type.enum(*args) ; Enum.new(*args).as Type end
    def to_s(io : IO)
      io << "enum #{@base.name}"
      io << "[#{@type_args.join(", ")}]" if @type_args.any?
    end
    def mode : Mode ; base.mode end
    # def substitute_Self_with(type : Type) : Type
    #   Enum.new(@base, @type_args.map { |ta| ta.substitute_Self_with(type) })
    # end
  end

  struct Function < Type
    getter args : Slice(Type)
    getter return_type : Cell(Type)
    def initialize(@args, @return_type)
    end
    def initialize(@args, return_type : Type)
      @return_type = Cell.new(return_type.as(Type))
    end
    def Type.function(*args) ; Function.new(*args).as Type end
    def to_s(io : IO)
      io << "(#{args.join(", ")}) -> #{@return_type.value}"
    end
    def mode : Mode; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Function.new(@args.map { |ta| ta.substitute_Self_with(type) }, @return_type.value.substitute_Self_with(type))
    # end
  end
end


# --- Type Information ---

struct TypeParameter < IrNode
  getter location : Location
  getter name : String
  getter required_traits : Slice(Trait)
  getter excluded_traits : Slice(Trait)
  def initialize(@location : Location, @name : String, @required_traits = Slice(Trait).empty, @excluded_traits = Slice(Trait).empty)
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

abstract class TypeInfo
  getter location : Location
  getter mode : Mode
  getter name : String
  property type_params : Slice(TypeParameter)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty)
  end
end

class StructBase < TypeInfo
  getter fields : ::Array(Field)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @fields : ::Array(Field) = [] of Field)
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

class EnumBase < TypeInfo
  getter variants : ::Array(Variant)
  def initialize(@location : Location, @mode : Mode, @name : String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @variants : ::Array(Variant) = [] of Variant)
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

class TraitBase < TypeInfo
  getter methods : ::Array(Ast::AbstractMethod)
  def initialize(@location : Location, @mode : Mode, @name : String, type_params : Slice(TypeParameter)? = nil, @methods : ::Array(Ast::AbstractMethod) = [] of Ast::AbstractMethod)
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
#   getter type_params : Slice(TypeParameter)
#   getter args : ::Array(Parameter)
#   getter return_type : Type
#   def initialize(@location : Location, @name : String, @type_params : Slice(TypeParameter), @args : ::Array(Parameter), @return_type : Type)
#   end
# end

class FunctionBase
  getter location : Location
  getter name : String
  property type_params : Slice(TypeParameter)
  property parameters : ::Array(Parameter) = [] of Parameter
  property return_mode : Mode = Mode::Move
  property return_type : Type = Type.nil
  property body : ::Array(Hir) = [] of Hir
  def initialize(@location : Location, @name : String, @type_params : Slice(TypeParameter), @parameters : ::Array(Parameter) = [] of Parameter, @return_type : Type = Type.nil)
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
end

struct Parameter < IrNode
  getter location : Location
  getter mode : Mode
  getter name : String
  getter type : Type
  def initialize(@location : Location, @mode : Mode, @name : String, @type : Type)
  end
  def to_s(io : IO)
    io << "#{mode} #{name}: #{type}"
  end
end

struct Trait
  getter base : TraitBase
  getter type_args : Slice(Type)
  def initialize(@base : TraitBase, @type_args : Slice(Type) = Slice(Type).empty)
    if @base.type_params.size != @type_args.size
      raise "wrong number of type args: #{inspect}"
    end
  end
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
  def inspect(io : IO)
    io << "Trait(#{@base.name}"
    io << "[#{type_args.join(", ")}]" if type_args.any?
    io << ")"
  end
end