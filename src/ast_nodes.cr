require "./mode_convention"
require "./tokens"
require "auto_constructor"

module Ast
  abstract struct Node
    property loc : Location
    def initialize(@loc : Location)
    end
    abstract def to_s(io : IO)

    def inspect(io : IO)
      io << {{@type.name.stringify}} << '(' << loc
      {% for ivar, i in @type.instance_vars %}
        # io << {{ivar.name.stringify}} << "="
        {% if ivar.name.id != "loc" %}
          io << {{ivar.name.id}}.inspect
        {% end %}
        {% if i < @type.instance_vars.size - 1 %}
          io << ", "
        {% end %}
      {% end %}
      io << ')'
    end

    def ==(other : self)
      {% for ivar in @type.instance_vars %}
        {% if ivar.name.id != "loc" %}
          return false unless other.responds_to?(:{{ivar.name.id}})
          return false unless {{ivar.name.id}} == other.{{ivar.name.id}}
        {% end %}
      {% end %}
      true
    end
  end

  struct Nil < Node
    def to_s(io : IO)
      io << "nil"
    end
    # def_eq
  end
  struct False < Node
    def to_s(io : IO)
      io << "false"
    end
  end
  struct True < Node
    def to_s(io : IO)
      io << "true"
    end
  end
  struct Int < Node
    property value : Int128
    def initialize(@loc : Location, @value : Int128)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct Float < Node
    property value : Float64
    def initialize(@loc : Location, @value : Float64)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct String < Node
    property value : ::String
    def initialize(@loc : Location, @value : ::String)
    end
    def to_s(io : IO)
      io << "\"#{value}\""
    end
  end
  struct Let < Node
    property name : ::String
    property value : Box(Node)
    def initialize(@loc : Location, @name : ::String, @value : Box(Node))
    end
    def initialize(@loc : Location, @name : ::String, value : Node)
      @value = Box.new(value.as(Node))
    end
    def to_s(io : IO)
      io << "let #{name} = #{value}"
    end
  end
  struct Var < Node
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Const < Node
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Identifier < Node
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Tuple < Node
    property elements : ::Array(Node)
    def initialize(@loc : Location, @elements : ::Array(Node))
    end
    def to_s(io : IO)
      io << "(#{elements.join(", ")})"
    end
  end
  struct Array < Node
    property elements : ::Array(Node)
    def initialize(@loc : Location, @elements : ::Array(Node))
    end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end
  struct EmptyTuple < Node
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "()"
    end
  end
  struct EmptyArray < Node
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "[]"
    end
  end
  struct Block < Node
    property statements : ::Array(Node)
    def initialize(@loc : Location, @statements : ::Array(Node))
    end
    def to_s(io : IO)
      io << "#{statements.join("\n")}"
    end
  end
  struct Type < Node
    property name : ::String
    property type_args : ::Array(Type)?
    def initialize(@loc : Location, @name : ::String, @type_args : ::Array(Type)? = nil)
    end
    def to_s(io : IO)
      io << name
      io << "[#{type_args.join(", ")}]" if type_args
    end
    # def_eq
    # def_inspect
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{loc})"
    # end
  end
  struct Binop < Node
    include AutoConstructor
    field :loc, Location
    field :left, Box(Node)
    field :operator, Operator
    field :right, Box(Node)
    def initialize(@loc : Location, left : Node, @operator : Operator, right : Node)
      @left = Box.new(left.as(Node))
      @right = Box.new(right.as(Node))
    end
    def to_s(io : IO)
      io << "#{left} #{self.class.name} #{right}"
    end
  end
  struct NotNode < Node
    property value : Box(Node)
    def initialize(@loc : Location, @value : Box(Node))
    end
    def initialize(@loc : Location, value : Node)
      @value = Box.new(value.as(Node))
    end
    def to_s(io : IO)
      io << "!#{value}"
    end
  end
  struct NegNode < Node
    property value : Box(Node)
    def initialize(@loc : Location, @value : Box(Node))
    end
    def initialize(@loc : Location, value : Node)
      @value = Box.new(value.as(Node))
    end
    def to_s(io : IO)
      io << "-#{value}"
    end
  end
  struct DotNode < Node
    property left : Box(Node)?
    property method : :: String
    def initialize(@loc : Location, @left : Box(Node)?, @method : :: String)
    end
    def initialize(@loc : Location, left : Node, @method : :: String)
      @left = Box.new(left.as(Node))
    end
    def initialize(@loc : Location, @method : :: String)
    end
    def to_s(io : IO)
      io << "#{left}.#{method}"
    end
  end
  struct Call < Node
    property receiver : Box(Node)
    property method : ::String
    property args : ::Array(Node)
    def initialize(@loc : Location, @receiver : Box(Node), @method : ::String, @args : ::Array(Node))
    end
    def initialize(@loc : Location, receiver : Node, @method : ::String, args : ::Array(Node))
      @receiver = Box.new(receiver.as(Node))
      @args = args.map { |arg| arg.as(Node) }
    end
    def to_s(io : IO)
      io << "#{receiver}.#{method}#{args.join(", ")}" 
    end
  end
  struct If < Node
    property condition : Box(Node)
    property then_branch : Box(Node)
    property else_branch : Box(Node)?
    def initialize(@loc : Location, @condition : Box(Node), @then_branch : Box(Node), @else_branch : Box(Node)?)
    end
    def initialize(@loc : Location, condition : Node, then_branch : Node, else_branch : Node?)
      @condition = Box.new(condition.as(Node))
      @then_branch = Box.new(then_branch.as(Node))
      @else_branch = Box.new(else_branch.as(Node)) if else_branch
    end
    def to_s(io : IO)
      io << "if #{condition}\n#{then_branch}\nelse\n#{else_branch}\nend"
    end
  end
  struct ForLoop < Node
    property variable : Box(Node)
    property collection : Box(Node)
    property body : Box(Node)
    def initialize(@loc : Location, @variable : Box(Node), @collection : Box(Node), @body : Box(Node))
    end
    def initialize(@loc : Location, variable : Node, collection : Node, body : Node)
      @variable = Box.new(variable.as(Node))
      @collection = Box.new(collection.as(Node))
      @body = Box.new(body.as(Node))
    end
    def to_s(io : IO)
      io << "for #{variable} in #{collection}\n#{body}\nend"
    end
  end
  struct WhileLoop < Node
    property condition : Box(Node)
    property body : Box(Node)
    def initialize(@loc : Location, @condition : Box(Node), @body : Box(Node))
    end
    def initialize(@loc : Location, condition : Node, body : Node)
      @condition = Box.new(condition.as(Node))
      @body = Box.new(body.as(Node))
    end
    def to_s(io : IO)
      io << "while #{condition}\n#{body}\nend"
    end
  end
  struct Return < Node
    property value : Box(Node)
    def initialize(@loc : Location, @value : Box(Node))
    end
    def initialize(@loc : Location, value : Node)
      @value = Box.new(value.as(Node))
    end
    def to_s(io : IO)
      io << "return #{value}"
    end
  end
  struct Break < Node
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "break"
    end
  end
  struct Continue < Node
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "continue"
    end
  end
  struct Error < Node
    property message : ::String
    def initialize(@loc : Location, @message : ::String)
    end
    def to_s(io : IO)
      io << "Error(#{message})"
    end
  end

  struct Import < Node
    property path : ::String
    property module_alias : ::String?
    property bindings : ::Array({::String, ::String})?

    def initialize(
      @loc : Location, 
      @path : ::String, 
      @module_alias : ::String?, 
      @bindings : ::Array({::String, ::String})?
      )
    end

    def to_s(io : IO)
      io << "import #{path}"
      io << " as #{module_alias}" if module_alias
      if bindings
        io << " " << bindings 
        # io << " {"
        # io << bindings.map { |b| "#{b[0]}: #{b[1]}" }.join(", ")
        # io << "}"
      end
    end
  end

  abstract struct TopLevelItem < Node
    property name : ::String
    property type_parameters : ::Array(Type)?
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?)
    end
    abstract def to_s(io : IO)
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{loc})"
    # end
  end

  abstract struct TypeDeclaration < TopLevelItem
    property convention : Convention
    property traits : ::Array(Type)?
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @convention : Convention, @traits : ::Array(Type)?)
    end
  end

  
  struct Struct < TypeDeclaration
    property fields : ::Array(Node)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @convention : Convention, @traits : ::Array(Type)?, @fields : ::Array(Node))
    end
    def to_s(io : IO)
      io << "struct #{name}"
    end
  end

  struct Enum < TypeDeclaration
    property variants : ::Array(Node)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @convention : Convention, @traits : ::Array(Type)?, @variants : ::Array(Node))
    end
    def to_s(io : IO)
      io << "enum #{name}"
    end
  end

  struct Trait < TypeDeclaration
    property methods : ::Array(Node)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @convention : Convention, @traits : ::Array(Type)?, @methods : ::Array(Node))
    end
    def to_s(io : IO)
      io << "trait #{name}"
    end
  end

  struct Extend < TopLevelItem
    property type : Type
    property traits : ::Array(Type)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @type : Type, @traits : ::Array(Type))
    end
    def to_s(io : IO)
      io << "extend #{name}"
    end
  end

  struct Function < TopLevelItem
    property signature : Signature
    property body : ::Array(Node)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @signature : Signature, @body : Array(Node))
    end
    def to_s(io : IO)
      io << "#{self.class.name}(#{name})"
    end
  end

  struct Signature < Node
    property type_parameters : ::Array(TypeParameter)?
    property params : ::Array(Parameter)?
    property return_type : Type?
    def initialize(@loc : Location, @type_parameters : ::Array(TypeParameter)? = nil, @params : ::Array(Parameter)? = nil, @return_type : Type? = nil)
    end
    def to_s(io : IO)
      io << "#{params.join(", ")} -> #{return_type}"
    end
  end

  struct TypeParameter < Node
    include AutoConstructor
    field :loc, Location
    field :name, ::String
    field :constraints, ::Array(Type)?
    # property loc : Location
    # property name : ::String
    # property constraints : ::Array(Type)? = nil
    # def initialize(@loc : Location, @name : ::String, @constraints : ::Array(Type)? = nil)
    # end
    def to_s(io : IO)
      io << "#{name}"
      io << ": " << constraints.join(", ") if constraints
    end
    # def inspect(io : IO)
    #   super
    # end
  end

  struct Parameter < Node
    property loc : Location
    property name : ::String
    property type : Type
    property convention : Convention
    def initialize(@loc : Location, @name : ::String, @type : Type, @convention : Convention = nil)
    end
    def initialize(@loc : Location, @convention : Convention, @name : ::String, @type : Type)
    end
    def to_s(io : IO)
      io << "#{name}: #{type}"
    end
  end

  class Box(T)
    property value : T
    def initialize(@value : T)
    end
    def ==(other : Box(T))
      value == other.value
    end
    def inspect(io : IO)
      value.inspect(io)
    end
    def to_s(io : IO)
      value.to_s(io)
    end
  end
end

