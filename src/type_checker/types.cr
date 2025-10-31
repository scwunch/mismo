require "../utils/cell"
# require "../ast/abstract_node"

abstract struct Type

  abstract def to_s(io : IO)
  abstract def mode : Mode
  def primitive? : ::Bool; false end
  def copy_type? : ::Bool; false end
  def generic? : ::Bool
    type_args?.try &.any?
  end
  def type_args? : ::Slice(Type)?
    if self.responds_to?(:type_args)
      self.type_args
    else
      nil
    end
  end
  def base_or_class
    if self.responds_to?(:base)
      self.base
    else
      self.class
    end
  end
  def is_enum?; false end
  def is_struct?; false end
  def type_def? : ::TypeDefinition?
    nil
  end

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
    def primitive? : ::Bool; true end
    def copy_type? : ::Bool; true end
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

  struct Any < Type
    primitive
  end

  struct Nil < Type
    primitive
  end

  struct Bool < Type
    primitive
  end

  struct Byte < Type
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

  # struct String < Type
  #   def Type.string ; String.new.as Type end
  #   def to_s(io : IO)
  #     io << "String"
  #   end
  #   def mode : Mode ; Mode::Ref end
  # end
  
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
    getter type_args : ::Slice(Type)
    def initialize(@name : ::String, @type_args : ::Slice(Type) = ::Slice(Type).empty)
    end
    def Type.unknown(name, type_args = ::Slice(Type).empty)
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

  module BoxyType
    getter element_type : Cell(Type)
    def initialize(@element_type : Cell(Type))
    end
    def initialize(element_type : Type)
      @element_type = Cell.new(element_type.as(Type))
    end
    def type_args : ::Slice(Type)
      ::Slice[element_type.value]
    end
    def mode : Mode ; Mode::Let end
    def to_s(io : IO)
      io << "#{ {{ @type.name.stringify }} }[#{@element_type}]"
    end
  end

  # struct Array < Type
  #   include BoxyType
  #   def Type.array(type) ; Array.new(type.as(Type)).as(Type) end
  # end

  struct Pointer < Type
    include BoxyType
    def mode : Mode ; Mode::Move end
    def primitive? : ::Bool; true end
    def copy_type? : ::Bool; true end
    def Type.pointer(type) ; Pointer.new(type.as(Type)).as(Type) end
  end

  struct Slice < Type
    include BoxyType
    def Type.slice(type) ; Slice.new(type.as(Type)).as(Type) end
    def mode : Mode ; Mode::Move end
    def primitive? : ::Bool; true end
    def copy_type? : ::Bool; true end
  end

  struct Tuple < Type
    getter types : ::Slice(Type)
    def initialize(@types : ::Slice(Type))
    end
    def initialize(types : ::Array(Type))
      @types = types.to_unsafe_slice
    end
    def Type.tuple(*args) ; Tuple.new(*args).as Type end
    def copy_type? : Bool
      types.all? &.copy_type?
    end
    def type_args : ::Slice(Type)
      types
    end
    def to_s(io : IO)
      io << "Tuple[#{@types.join(", ")}]"
    end
    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Tuple.new(@types.map { |t| t.substitute_Self_with(type) })
    # end
  end

  struct Union < Type
    getter types : ::Slice(Type)
    def initialize(@types : ::Slice(Type))
    end
    def initialize(types : ::Array(Type))
      @types = types.to_unsafe_slice
    end
    def type_args : ::Slice(Type)
      types
    end
    def Type.union(one_type : Type)
      Union.new(Slice[one_type]).as Type
    end
    def Type.union(types : Iterator(Type))
      types = types.uniq.select { |t| t != Type.never }.to_a
      case types.size
      when 0 then Type.never
      when 1 then types.first.as Type
      else 
        Union.new(types.to_unsafe_slice).as Type
      end
    end
    def to_s(io : IO)
      io << "Union[#{@types.join(", ")}]"
    end
    def mode : Mode ; Mode::Let end
    # def substitute_Self_with(type : Type) : Type
    #   Union.new(@types.map { |t| t.substitute_Self_with(type) })
    # end
  end

  # def Type.adt(base : StructDef, type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   Struct.new(base, type_args).as Type
  # end

  # def Type.adt(base : EnumDef, type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   Enum.new(base, type_args).as Type
  # end

  # def Type.adt(base : TypeDefinition, type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   case base
  #   when StructDef then Struct.new(base, type_args)
  #   when EnumDef then Enum.new(base, type_args)
  #   else
  #     raise "Type.adt: unknown type info: #{base}"
  #   end.as Type
  # end

  struct Adt < Type
    getter base : TypeDefinition
    getter type_args : ::Slice(Type)
    def initialize(@base : TypeDefinition, @type_args : ::Slice(Type))
    end
    def Type.adt(base : TypeDefinition, type_args : ::Slice(Type) = ::Slice(Type).empty)
      Adt.new(base, type_args).as Type
    end
    def is_enum?
      base.is_a?(EnumDef)
    end
    def is_struct?
      base.is_a?(StructDef)
    end
    def type_def? : ::TypeDefinition?
      @base
    end
    def to_s(io : IO)
      io << base.name
      io << "[#{@type_args.join(", ")}]" if @type_args.any?
    end
    def mode : Mode ; base.mode end
    def get_field?(field_name : ::String) : Field?
      b = base
      unless b.is_a?(StructDef)
        raise "Adt#get_field?: not implemented yet for enums (#{self})"
      end
      b.fields.each do |field|
        return field if field.name == field_name
      end
      nil
    end
    # def substitute_Self_with(type : Type) : Type
    #   Adt.new(@type_args.map { |ta| ta.substitute_Self_with(type) })
    # end
  end

  # struct Struct < Type
  #   getter base : StructDef
  #   getter type_args : ::Slice(Type)
  #   def initialize(@base : StructDef, @type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   end
  #   def Type.struct(*args) ; Struct.new(*args).as Type end
  #   def to_s(io : IO)
  #     io << "struct #{@base.name}"
  #     io << "[#{@type_args.join(", ")}]" if @type_args.any?
  #   end
  #   def inspect(io : IO)
  #     io << "Type::Struct("
  #     base.inspect(io)
  #     io << "[#{@type_args.join(", ")}]" if @type_args.any?
  #     io << ")"
  #   end
  #   def mode : Mode ; base.mode end
  #   # def substitute_Self_with(type : Type) : Type
  #   #   Struct.new(@base, @type_args.map { |ta| ta.substitute_Self_with(type) })
  #   # end
  #   def get_field?(field_name : ::String) : Field?
  #     base.fields.each do |field|
  #       return field if field.name == field_name
  #     end
  #     nil
  #   end
  # end

  # struct Enum < Type
  #   getter base : EnumDef
  #   getter type_args : ::Slice(Type)
  #   def initialize(@base : EnumDef, @type_args : ::Slice(Type) = ::Slice(Type).empty)
  #   end
  #   def Type.enum(*args) ; Enum.new(*args).as Type end
  #   def to_s(io : IO)
  #     io << "enum #{@base.name}"
  #     io << "[#{@type_args.join(", ")}]" if @type_args.any?
  #   end
  #   def mode : Mode ; base.mode end
  #   # def substitute_Self_with(type : Type) : Type
  #   #   Enum.new(@base, @type_args.map { |ta| ta.substitute_Self_with(type) })
  #   # end
  # end

  struct Function < Type
    getter args : ::Slice(Type)
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