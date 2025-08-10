require "./mode_convention"
require "./tokens"
require "./types"
require "./cell"
require "./abstract_node"

abstract struct Hir < IrNode
  abstract def location : Location
  abstract def binding : Binding
  abstract def type : Type
  abstract def to_s(io : IO)

  # eg `Hir.int(454)` => `Hir::Int.new(454).as Hir`
  macro def_init
    def Hir.{{@type.name[5..].downcase}}(*args)
      {{@type.name[5..]}}.new(*args).as Hir
    end
  end

  def mutable?
    binding == Binding::Mut || binding == Binding::Var
  end

  struct Nil < Hir
    property location : Location
    def initialize(@location : Location)
    end
    def_init
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.nil end
    def to_s(io : IO)
      io << "nil"
    end
  end

  struct False < Hir
    property location : Location
    def initialize(@location : Location)
    end
    def_init
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.bool end
    def to_s(io : IO)
      io << "false"
    end
  end

  struct True < Hir
    property location : Location
    def initialize(@location : Location)
    end
    def_init
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.bool end
    def to_s(io : IO)
      io << "true"
    end
  end

  struct Int < Hir
    property location : Location
    property value : Int128
    def initialize(@location : Location, @value : Int128)
    end
    def_init
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.int end
    def to_s(io : IO)
      io << value.to_s
    end
  end

  struct Float < Hir
    property location : Location
    property value : Float64
    def initialize(@location : Location, @value : Float64)
    end
    def_init
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.float end
    def to_s(io : IO)
      io << value.to_s
    end
  end

  struct String < Hir
    property location : Location
    property value : ::String
    def initialize(@location : Location, @value : ::String)
    end
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.string end
    def to_s(io : IO)
      io << "\"#{value}\""
    end
  end

  struct Identifier < Hir
    property location : Location
    property name : ::String
    property binding : Binding
    property type : Type
    def initialize(@location : Location, @name : ::String, @binding : Binding, @type : Type)
    end
    def to_s(io : IO)
      io << name
    end
  end

  struct Array < Hir
    property location : Location
    property element_type : Type
    property elements : ::Array(Hir)
    def initialize(@location : Location, @element_type : Type, @elements : ::Array(Hir))
    end
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.array(element_type) end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end

  struct Tuple < Hir
    property location : Location
    property elements : ::Array(Hir)
    def initialize(@location : Location, @elements : ::Array(Hir))
    end
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.tuple(@elements.map &.type) end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end

  # no neg, not, or binop — all of those are smushed into Call

  struct Call < Hir
    property location : Location
    property function : FunctionBase
    property args : ::Array(Hir)
    getter type : Type
    def initialize(@location : Location, @function : FunctionBase, @args : ::Array(Hir), @type : Type)
    end
    def binding : Binding
      function.return_mode.to_binding
    end
    def to_s(io : IO)
      io << "#{function.name}(#{args.join(", ")})"
    end
  end

  struct Assign < Hir
    property location : Location
    property variable : ::Variable
    property value : Cell(Hir)
    def initialize(@location : Location, @variable : ::Variable, @value : Cell(Hir))
    end
    def initialize(@location : Location, @variable : ::Variable, value : Hir)
      @value = Cell.new(value.as(Hir))
    end
    def binding : Binding
      variable.binding
    end
    def type : Type 
      variable.type || Type.nil
    end
    def to_s(io : IO)
      io << "#{variable.name} = #{value}"
    end
  end

  struct AssignField < Hir
    property location : Location
    property object : Cell(Hir)
    property field : ::Field
    property value : Cell(Hir)
    def initialize(@location : Location, @object : Cell(Hir), @field : ::Field, @value : Cell(Hir))
    end
    def initialize(@location : Location, object : Hir, @field : ::Field, value : Hir)
      @object = Cell.new(object.as(Hir))
      @value = Cell.new(value.as(Hir))
    end
    def binding : Binding
      field.binding
    end
    def type : Type 
      field.type
    end
    def to_s(io : IO)
      io << "#{object}.#{field.name} = #{value.value}"
    end
  end

  struct Let < Hir
    property location : Location
    property name : ::String
    property value : Cell(Hir)?
    def initialize(@location : Location, @name : ::String, @value : Cell(Hir)? = nil)
    end
    def initialize(@location : Location, @name : ::String, value : Hir)
      @value = Cell.new(value.as(Hir))
    end
    def binding : Binding
      Binding::Var
    end
    def type : Type ; Type.nil end
    def to_s(io : IO)
      io << "let #{name} = #{value}"
    end
  end

  struct Var < Hir
    property location : Location
    property name : ::String
    property value : Cell(Hir)?
    def initialize(@location : Location, @name : ::String, @value : Cell(Hir)? = nil)
    end
    def initialize(@location : Location, @name : ::String, value : Hir)
      @value = Cell.new(value.as(Hir))
    end
    def binding : Binding
      Binding::Var
    end
    def type : Type ; Type.nil end
    def to_s(io : IO)
      io << "var #{name} = #{value}"
    end
  end
end