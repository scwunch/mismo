require "./wat"

module Wasm
  # type Node = Atom | Array(Node)
  abstract class Node
    abstract def wat : Wat
    def fmt(formatter : Wat::Formatter = Wat::Formatter.new(STDOUT))
      wat.fmt(formatter)
    end
  end

  class Module < Node
    property body : Array(Node)
    def initialize(@body : Array(Node))
    end
    def wat : Wat
      arr = [Wat.new("module")]
      arr.concat(body.map &.wat)
      Wat.new(arr)
    end
  end

  class Import < Node
    property source : String
    property name : String
    def initialize(@source : String, @name : String)
    end
    def wat : Wat
      Wat.new [
        Wat.new("import"),
        Wat.new("\"#{source}\""),
        Wat.new("\"#{name}\""),
        Wat.new("(func 0)"),
      ]
    end
  end

  class Function < Node
    property name : Identifier?
    property export : Export?
    property params : Array(Param)
    property result : Result?
    property body : Array(Node)
    def initialize(@name : Identifier?, @export : Export?, @params : Array(Param), @result : Result?, @body : Array(Node))
    end
    def wat : Wat
      arr = [Wat.new("func")]
      if n = name
        arr << n.wat
      end
      if e = export
        arr << e.wat
      end
      arr.concat(params.map &.wat)
      if r = result
        arr << r.wat
      end
      arr.concat(body.map &.wat)
      Wat.new(arr)
    end
  end

  class Result < Node
    property type : Type
    def initialize(@type : Type)
    end
    def wat : Wat
      Wat.new([
        Wat.new("result"),
        type.wat
      ])
    end
  end

  class Export < Node
    property name : String
    def initialize(@name : String)
    end
    def wat : Wat
      Wat.new [
        Wat.new("export"),
        Wat.new("\"#{name}\""),
      ]
    end
  end

  class Type < Node
    property name : Symbol
    def initialize(@name : Symbol)
    end
    def wat : Wat
      Wat.new(name.to_s)
    end
  end

  class Param < Node
    property name : Identifier
    property type : Type
    def initialize(@name : Identifier, @type : Type)
    end
    def wat : Wat
      Wat.new [
        Wat.new("param"),
        name.wat,
        type.wat,
      ]
    end
  end

  class Identifier < Node
    property name : String
    def initialize(@name : String)
    end
    def wat : Wat
      Wat.new("$#{name}")
    end
  end

  class Add < Node
    property left : Node
    property right : Node
    def initialize(@left : Node, @right : Node)
    end
    def wat : Wat
      Wat.new([
        Wat.new("add"),
        left.wat,
        right.wat,
      ])
    end
  end

  class AddAtom < Node
    property type : Symbol
    def initialize(@type)
    end
    def wat : Wat
      Wat.new("#{type}.add")
    end
  end

  class Const < Node
    property type : Symbol
    property value : Int32 | Int64 | Float32 | Float64
    def initialize(@type, @value)
    end
    def initialize(@value)
      # @type = value.class.to_s.downcase.to_sym
      @type = case @value
      when Int32
        :i32
      when Int64
        :i64
      when Float32
        :f32
      when Float64
        :f64
      else
        raise "Unsupported constant type: #{@value.class}"
      end
    end
    def wat : Wat
      Wat.new([
        Wat.new("#{type}.const"),
        Wat.new(value.to_s)
      ])
    end
  end
end