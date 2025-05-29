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

  abstract struct Expr < Node
  end

  struct Nil < Expr
    def to_s(io : IO)
      io << "nil"
    end
    # def_eq
  end
  struct False < Expr
    def to_s(io : IO)
      io << "false"
    end
  end
  struct True < Expr
    def to_s(io : IO)
      io << "true"
    end
  end
  struct Int < Expr
    property value : Int128
    def initialize(@loc : Location, @value : Int128)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct Float < Expr
    property value : Float64
    def initialize(@loc : Location, @value : Float64)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct String < Expr
    property value : ::String
    def initialize(@loc : Location, @value : ::String)
    end
    def to_s(io : IO)
      io << "\"#{value}\""
    end
  end
  struct Let < Expr
    property name : ::String
    property value : Box(Expr)
    def initialize(@loc : Location, @name : ::String, @value : Box(Expr))
    end
    def initialize(@loc : Location, @name : ::String, value : Expr)
      @value = Box.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "let #{name} = #{value}"
    end
  end
  struct Var < Expr
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Const < Expr
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Identifier < Expr
    property name : ::String
    def initialize(@loc : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Tuple < Expr
    property elements : ::Array(Expr)
    def initialize(@loc : Location, @elements : ::Array(Expr))
    end
    def to_s(io : IO)
      io << "(#{elements.join(", ")})"
    end
  end
  struct NamedTuple < Expr
    property fields : ::Array({::String, Expr})
    def initialize(@loc : Location, @fields : ::Array({::String, Expr}))
    end
    def to_s(io : IO)
      io << "(#{fields.map { |f| "#{f[0]}: #{f[1]}" }.join(", ")})"
    end
  end
  struct Array < Expr
    property elements : ::Array(Expr)
    def initialize(@loc : Location, @elements : ::Array(Expr))
    end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end
  struct EmptyTuple < Expr
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "()"
    end
  end
  struct EmptyArray < Expr
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "[]"
    end
  end
  # struct Block < Node
  #   property statements : ::Array(Node)
  #   def initialize(@loc : Location, @statements : ::Array(Node))
  #   end
  #   def to_s(io : IO)
  #     io << "#{statements.join("\n")}"
  #   end
  # end
  struct Type < Expr
    property name : ::String
    property type_args : ::Array(Type)?
    def initialize(@loc : Location, @name : ::String, @type_args : ::Array(Type)? = nil)
    end
    def to_s(io : IO)
      io << name
      if t_args = type_args
        io << "[#{t_args.join(", ")}]"
      end
    end
    # def_eq
    # def_inspect
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{loc})"
    # end
  end
  struct Binop < Expr
    include AutoConstructor
    field :loc, Location
    field :left, Box(Expr)
    field :operator, Operator
    field :right, Box(Expr)
    def initialize(@loc : Location, left : Expr, @operator : Operator, right : Expr)
      @left = Box.new(left.as(Expr))
      @right = Box.new(right.as(Expr))
    end
    def to_s(io : IO)
      io << "#{left} #{self.class.name} #{right}"
    end
  end
  struct NotNode < Expr
    property value : Box(Expr)
    def initialize(@loc : Location, @value : Box(Expr))
    end
    def initialize(@loc : Location, value : Expr)
      @value = Box.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "!#{value}"
    end
  end
  struct NegNode < Expr
    property value : Box(Expr)
    def initialize(@loc : Location, @value : Box(Expr))
    end
    def initialize(@loc : Location, value : Expr)
      @value = Box.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "-#{value}"
    end
  end
  struct DotNode < Expr
    property left : Box(Expr)?
    property method : :: String
    def initialize(@loc : Location, @left : Box(Expr)?, @method : :: String)
    end
    def initialize(@loc : Location, left : Expr, @method : :: String)
      @left = Box.new(left.as(Expr))
    end
    def initialize(@loc : Location, @method : :: String)
    end
    def to_s(io : IO)
      io << "#{left}.#{method}"
    end
  end
  struct Call < Expr
    property receiver : Box(Expr)
    property method : ::String
    property args : ::Array(Expr)
    def initialize(@loc : Location, @receiver : Box(Expr), @method : ::String, @args : ::Array(Expr))
    end
    def initialize(@loc : Location, receiver : Expr, @method : ::String, args : ::Array(Expr))
      @receiver = Box.new(receiver.as(Expr))
      @args = args.map { |arg| arg.as(Expr) }
    end
    def to_s(io : IO)
      io << "#{receiver}.#{method}#{args.join(", ")}" 
    end
  end
  struct If < Expr
    property condition : Box(Expr)
    property then_branch : Box(Expr)
    property else_branch : Box(Expr)?
    def initialize(@loc : Location, @condition : Box(Expr), @then_branch : Box(Expr), @else_branch : Box(Expr)?)
    end
    def initialize(@loc : Location, condition : Expr, then_branch : Expr, else_branch : Expr?)
      @condition = Box.new(condition.as(Expr))
      @then_branch = Box.new(then_branch.as(Expr))
      @else_branch = Box.new(else_branch.as(Expr)) if else_branch
    end
    def to_s(io : IO)
      io << "if #{condition}\n#{then_branch}\nelse\n#{else_branch}\nend"
    end
  end
  struct ForLoop < Expr
    property variable : Box(Expr)
    property collection : Box(Expr)
    property body : Box(Expr)
    def initialize(@loc : Location, @variable : Box(Expr), @collection : Box(Expr), @body : Box(Expr))
    end
    def initialize(@loc : Location, variable : Expr, collection : Expr, body : Expr)
      @variable = Box.new(variable.as(Expr))
      @collection = Box.new(collection.as(Expr))
      @body = Box.new(body.as(Expr))
    end
    def to_s(io : IO)
      io << "for #{variable} in #{collection}\n#{body}\nend"
    end
  end
  struct WhileLoop < Expr
    property condition : Box(Expr)
    property body : Box(Expr)
    def initialize(@loc : Location, @condition : Box(Expr), @body : Box(Expr))
    end
    def initialize(@loc : Location, condition : Expr, body : Expr)
      @condition = Box.new(condition.as(Expr))
      @body = Box.new(body.as(Expr))
    end
    def to_s(io : IO)
      io << "while #{condition}\n#{body}\nend"
    end
  end
  struct Return < Expr
    property value : Box(Expr)
    def initialize(@loc : Location, @value : Box(Expr))
    end
    def initialize(@loc : Location, value : Expr)
      @value = Box.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "return #{value}"
    end
  end
  struct Break < Expr
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "break"
    end
  end
  struct Continue < Expr
    def initialize(@loc : Location)
    end
    def to_s(io : IO)
      io << "continue"
    end
  end
  struct Error < Expr
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
    property type_parameters : ::Array(TypeParameter)?
    def initialize(@loc : Location, @type_parameters : ::Array(TypeParameter)? = nil)
    end
    abstract def to_s(io : IO)
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{loc})"
    # end
  end

  abstract struct TypeDeclaration < TopLevelItem
    property convention : Convention
    property name : ::String
    property traits : ::Array(Type)?
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(Type)?, @convention : Convention, @traits : ::Array(Type)?)
    end
  end

  
  struct Struct < TypeDeclaration
    property fields : ::Array(Field)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @fields : ::Array(Field) = [] of Field, @convention : Convention = nil)
    end
    def initialize(@loc : Location, @convention : Convention, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @fields : ::Array(Field) = [] of Field)
    end
    def to_s(io : IO)
      io << "struct #{name}"
    end
  end

  struct Field < Node
    property name : ::String
    property type : Type
    property init : Expr?
    def initialize(@loc : Location, @name : ::String, @type : Type, @init : Expr? = nil)
    end
    def to_s(io : IO)
      io << "field #{name}: #{type}"
      io << " = #{init}" if init
    end
  end

  struct Enum < TypeDeclaration
    property variants : ::Array(Variant)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @variants : ::Array(Variant) = [] of Variant, @convention : Convention = nil)
    end
    def initialize(@loc : Location, @convention : Convention, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @variants : ::Array(Variant) = [] of Variant)
    end
    def to_s(io : IO)
      io << "enum #{name}"
    end
  end

  struct Variant < Node
    property name : ::String
    property fields : NamedTuple
    def initialize(@loc : Location, @name : ::String, @fields : NamedTuple)
    end
    def to_s(io : IO)
      io << "variant #{name}#{fields}"
    end
  end

  struct Trait < TypeDeclaration
    property methods : ::Array(Function)
    def initialize(@loc : Location, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @methods : ::Array(Function) = [] of Function, @convention : Convention = nil)
    end
    def initialize(@loc : Location, @convention : Convention, @name : ::String, @type_parameters : ::Array(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @methods : ::Array(Function) = [] of Function)
    end
    def to_s(io : IO)
      io << "trait #{name}"
    end
  end

  struct Extend < TopLevelItem
    property type : Type
    property traits : ::Array(Type)
    def initialize(@loc : Location, @type_parameters : ::Array(Type)?, @type : Type, @traits : ::Array(Type) = [] of Type)
    end
    def initialize(@loc : Location, @type : Type, *, @type_parameters : ::Array(Type)? = nil, @traits : ::Array(Type) = [] of Type)
    end
    def name 
      @type.name
    end
    def to_s(io : IO)
      io << "extend #{name}"
    end
  end

  struct Function < TopLevelItem
    property name : ::String
    property signature : Signature
    property body : ::Array(Expr)
    def initialize(@loc : Location, @name : ::String, @signature : Signature, @body : ::Array(Expr) = [] of Expr)
    end
    def to_s(io : IO)
      io << "#{self.class.name}(#{name})"
    end
  end

  struct Signature < Node
    property type_parameters : ::Array(TypeParameter)?
    property params : ::Array(Parameter)?
    property return_type : Type?
    property return_convention : Convention?
    def initialize(@loc : Location, @type_parameters : ::Array(TypeParameter)? = nil, @params : ::Array(Parameter)? = nil, @return_type : Type? = nil, @return_convention : Convention = nil)
    end
    def to_s(io : IO)
      io << "#{params.join(", ")} -> #{return_convention} #{return_type}"
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

  def self.to_type_args(type_params : ::Array(TypeParameter)?) : ::Array(Type)?
    return nil unless type_params
    type_params.map { |tp| Type.new(tp.loc, tp.name, tp.constraints) }
  end
end

