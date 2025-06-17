require "./mode_convention"
require "./tokens"
require "./types"
require "./cell"

abstract struct Hir
  abstract def location : Location
  property binding : Mode?
  abstract def type : Type
  abstract def to_s(io : IO)

  def ==(other)
    if other.is_a?(self)
      {% for ivar in @type.instance_vars %}
        {% if ivar.name.id != "location" %}
          return false unless @{{ivar.id}} == other.@{{ivar.id}}
        {% end %}
      {% end %}
      true
    else
      false
    end
  end

  struct Nil < Hir
    property location : Location
    def initialize(@location : Location)
    end
    def type : Type ; Type.nil end
    def to_s(io : IO)
      io << "nil"
    end
  end

  struct False < Hir
    property location : Location
    def initialize(@location : Location)
    end
    def type : Type ; Type.bool end
    def to_s(io : IO)
      io << "false"
    end
  end

  struct True < Hir
    property location : Location
    def initialize(@location : Location)
    end
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
    def type : Type ; Type.string end
    def to_s(io : IO)
      io << "\"#{value}\""
    end
  end

  struct Identifier < Hir
    property location : Location
    property name : ::String
    property type : Type
    def initialize(@location : Location, @name : ::String, @type : Type)
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
    def type : Type ; Type.tuple(@elements.map &.type) end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end

  # no neg, not, or binop — all of those are smushed into Call

  struct Call < Hir
    property location : Location
    property name : ::String
    property args : ::Array(Hir)
    getter type : Type
    def initialize(@location : Location, @name : ::String, @args : ::Array(Hir), @type : Type)
    end
    def to_s(io : IO)
      io << "#{name}([#{args.join(", ")}]"
    end
  end
end