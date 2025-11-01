require "../ast/mode_convention"
require "../ast/tokens"
require "../utils/cell"
require "../ast/abstract_node"

module Ast
  abstract struct Expr < IrNode
    def branching? : Bool
      false
    end
  end

  struct Nil < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "nil"
    end
  end
  struct False < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "false"
    end
  end
  struct True < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "true"
    end
  end
  struct Int < Expr
    property location : Location
    property value : Int128
    def initialize(@location : Location, @value : Int128)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct Float < Expr
    property location : Location
    property value : Float64
    def initialize(@location : Location, @value : Float64)
    end
    def to_s(io : IO)
      io << value.to_s
    end
  end
  struct String < Expr
    property location : Location
    property value : ::String
    def initialize(@location : Location, @value : ::String)
    end
    def to_s(io : IO)
      io << "\"#{value}\""
    end
  end
  struct Let < Expr
    property location : Location
    property name : ::String
    property value : Cell(Expr)?
    def initialize(@location : Location, @name : ::String, @value : Cell(Expr)? = nil)
    end
    def initialize(@location : Location, @name : ::String, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "let #{name} = #{value}"
    end
  end
  struct Var < Expr
    property location : Location
    property name : ::String
    property value : Cell(Expr)?
    def initialize(@location : Location, @name : ::String, @value : Cell(Expr)? = nil)
    end
    def initialize(@location : Location, @name : ::String, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "var #{name} = #{value}"
    end
  end
  struct Const < Expr
    property location : Location
    property name : ::String
    property value : Cell(Expr)?
    def initialize(@location : Location, @name : ::String, @value : Cell(Expr)? = nil)
    end
    def initialize(@location : Location, @name : ::String, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Identifier < Expr
    property location : Location
    property name : ::String
    def initialize(@location : Location, @name : ::String)
    end
    def to_s(io : IO)
      io << name
    end
  end
  struct Tuple < Expr
    property location : Location
    property elements : ::Array(Expr)
    def initialize(@location : Location, @elements : ::Array(Expr))
    end
    def initialize(@elements : ::Array(Expr))
      @location = @elements[0]?.try &.location || Location.zero
    end
    def to_s(io : IO)
      io << "(#{elements.join(", ")})"
    end
  end
  struct NamedTuple < Expr
    property location : Location
    property fields : ::Array({::String, Expr})
    def initialize(@location : Location, @fields : ::Array({::String, Expr}))
    end
    def to_s(io : IO)
      io << "(#{fields.map { |f| "#{f[0]}: #{f[1]}" }.join(", ")})"
    end
  end
  struct Array < Expr
    property location : Location
    property elements : ::Array(Expr)
    def initialize(@location : Location, @elements : ::Array(Expr))
    end
    def to_s(io : IO)
      io << "[#{elements.join(", ")}]"
    end
  end
  struct EmptyTuple < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "()"
    end
  end
  struct EmptyArray < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "[]"
    end
  end
  struct Block < Expr
    property location : Location
    property statements : ::Array(Expr)
    def initialize(@location : Location, @statements : ::Array(Expr))
    end
    def initialize(@statements)
      @location = @statements[0]?.try &.location || Location.zero
    end
    def to_s(io : IO)
      io << "#{statements.join("\n")}"
    end
  end
  struct Binop < Expr
    property left : Cell(Expr)
    property operator : Operator
    property right : Cell(Expr)
    def initialize(left : Expr, @operator : Operator, right : Expr)
      @left = Cell.new(left.as(Expr))
      @right = Cell.new(right.as(Expr))
    end
    def location : Location
      @left.value.location
    end
    def to_s(io : IO)
      io << "#{left} #{operator} #{right}"
    end
  end
  struct NotNode < Expr
    property location : Location
    property value : Cell(Expr)
    def initialize(@location : Location, @value : Cell(Expr))
    end
    def initialize(@location : Location, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "not#{value}"
    end
  end
  struct NegNode < Expr
    property location : Location
    property value : Cell(Expr)
    def initialize(@location : Location, @value : Cell(Expr))
    end
    def initialize(@location : Location, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "-#{value}"
    end
  end

  abstract struct ModeExpr < Expr
    abstract def mode : Mode
    property location : Location
    property expr : Cell(Expr)
    def initialize(@location : Location, @expr : Cell(Expr))
    end
    def initialize(@location : Location, expr : Expr)
      @expr = Cell.new(expr.as(Expr))
    end
    def to_s(io : IO)
      io << "#{mode} #{expr}"
    end
  end
  struct LetExpr < ModeExpr
    def mode : Mode ; Mode::Let end
  end
  struct MutExpr < ModeExpr
    def mode : Mode ; Mode::Mut end
  end
  struct MoveExpr < ModeExpr
    def mode : Mode ; Mode::Move end
  end
  # struct CopyExpr < ModeExpr
  #   def mode : Mode ; Mode::Copy end
  # end
  struct CellExpr < ModeExpr
    def mode : Mode ; Mode::Box end
  end
  struct RefExpr < ModeExpr
    def mode : Mode ; Mode::Ref end
  end

  # struct MethodCall < Expr
  #   # property receiver : Cell(Expr)
  #   # property call : Call
  #   # def initialize(@receiver : Cell(Expr), @call : Call)
  #   # end
  #   # def initialize(receiver : Expr, @call : Call)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   # end
  #   # def location : Location
  #   #   @receiver.value.location
  #   # end
  #   # def initialize(receiver : Expr, location : Location, method : ::String, type_args : Slice(Type) = nil, args : Args? = nil)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   #   @call = Call.new(location, method, type_args, args)
  #   # end
  #   # def initialize(receiver : Expr, location : Location, method : ::String, args : Args? = nil, type_args : Slice(Type) = nil)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   #   @call = Call.new(location, method, type_args, args)
  #   # end
  #   # def initialize(receiver : Expr, method : ::String, type_args : Slice(Type) = nil, args : Args? = nil)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   #   @call = Call.new(location, method, type_args, args)
  #   # end
  #   # def initialize(receiver : Expr, method : ::String, args : Args? = nil, type_args : Slice(Type) = nil)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   #   @call = Call.new(location, method, type_args, args)
  #   # end
  #   # def initialize(receiver : Expr, *args)
  #   #   @receiver = Cell.new(receiver.as(Expr))
  #   #   @call = Call.new(location, *args)
  #   # end
  #   # def method : ::String
  #   #   call.method
  #   # end
  #   # def type_args : Slice(Type)
  #   #   call.type_args
  #   # end
  #   # def args : Args?
  #   #   call.args
  #   # end
  #   # def to_s(io : IO)
  #   #   io << "#{receiver}."
  #   #   call.to_s(io)
  #   # end
  # end 

  # alias Args = ::Array({Convention, Expr})
  # changed because we now have ModeExpr
  alias Args = ::Array(Expr)

  struct Call < Expr
    property location : Location
    property method : ::String
    property type_args : Slice(Type)
    property args : Args?
    def initialize(
      @location : Location, 
      @method : ::String, 
      @type_args : Slice(Type), 
      @args : Args?)
    end
    def initialize(@location : Location, @method : ::String, @args : Args? = nil, @type_args : Slice(Type) = Slice(Type).empty)
    end
    # def initialize(@location : Location, @method : ::String, args : ::Array(Expr), @type_args : Slice(Type) = Slice(Type).empty)
    #   @args = args.map { |arg| {nil.as(Convention), arg.as(Expr)} }
    # end
    # def initialize(@location : Location, @method : ::String, @type_args : Slice(Type), args : ::Array(Expr))
    #   @args = args.map { |arg| {nil.as(Convention), arg.as(Expr)} }
    # end
    def initialize(@location : Location, @method : ::String, @type_args : Slice(Type) = Slice(Type).empty)
    end
    def initialize(@method : ::String, @args : Args? = nil, @type_args : Slice(Type) = Slice(Type).empty)
      if @type_args && @type_args.size > 0
        @location = @type_args[0].location -(@method.size + 1)
      elsif @args && @args.size > 0
        @location = @args[0].location - (@method.size + 1)
      else
        @location = Location.zero
      end
    end
    def to_s(io : IO)
      io << method
      if ta = type_args
        io << "[#{ta.join(", ")}]"
      end
      if a = args
        io << "(#{a.join(", ")})"
      end
    end
    def receiver? : Expr?
      if (a = args) && a.size > 0 && a[0].location == location
        a[0]
      end
    end
    def count_args
      if (a = args)
        a.size
      else
        0
      end
    end
  end

  struct Constructor < Expr
    property location : Location
    property type : Type
    property args : Args
    def initialize(@location : Location, @type : Type, @args : Args)
    end
    def to_s(io : IO)
      io << "#{type}"
      io << "(#{args.join(", ")})"
    end
  end

  struct StaticCall < Expr
    property location : Location
    property type : Type
    property call : Call
    def initialize(@location : Location, @type : Type, @call : Call)
    end
    def to_s(io : IO)
      io << "#{type}."
      call.to_s(io)
    end
  end
  
  # struct If < Expr
  #   property location : Location
  #   property condition : Cell(Expr)
  #   property then_branch : Cell(Expr)
  #   property else_branch : Cell(Expr)?
  #   def initialize(@location : Location, @condition : Cell(Expr), @then_branch : Cell(Expr), @else_branch : Cell(Expr)?)
  #   end
  #   def initialize(@location : Location, condition : Expr, then_branch : Expr, else_branch : Expr?)
  #     @condition = Cell.new(condition.as(Expr))
  #     @then_branch = Cell.new(then_branch.as(Expr))
  #     @else_branch = Cell.new(else_branch.as(Expr)) if else_branch
  #   end
  #   def to_s(io : IO)
  #     io << "if #{condition}\n#{then_branch}\nelse\n#{else_branch}\nend"
  #   end
  # end
  struct ForLoop < Expr
    property location : Location
    property variable : Cell(Expr)
    property collection : Cell(Expr)
    property body : Cell(Expr)
    def initialize(@location : Location, @variable : Cell(Expr), @collection : Cell(Expr), @body : Cell(Expr))
    end
    def initialize(@location : Location, variable : Expr, collection : Expr, body : Expr)
      @variable = Cell.new(variable.as(Expr))
      @collection = Cell.new(collection.as(Expr))
      @body = Cell.new(body.as(Expr))
    end
    def branching? : Bool
      true
    end
    def to_s(io : IO)
      io << "for #{variable} in #{collection}\n#{body}\nend"
    end
  end
  struct WhileLoop < Expr
    property location : Location
    property condition : Cell(Expr)
    property body : Cell(Expr)
    def initialize(@location : Location, @condition : Cell(Expr), @body : Cell(Expr))
    end
    def initialize(@location : Location, condition : Expr, body : Expr)
      @condition = Cell.new(condition.as(Expr))
      @body = Cell.new(body.as(Expr))
    end
    def branching? : Bool
      true
    end
    def to_s(io : IO)
      io << "while #{condition}\n#{body}\nend"
    end
  end
  struct Return < Expr
    property location : Location
    property value : Cell(Expr)
    def initialize(@location : Location, @value : Cell(Expr))
    end
    def initialize(@location : Location, value : Expr)
      @value = Cell.new(value.as(Expr))
    end
    def to_s(io : IO)
      io << "return #{value}"
    end
  end
  struct Break < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "break"
    end
  end
  struct Continue < Expr
    property location : Location
    def initialize(@location : Location)
    end
    def to_s(io : IO)
      io << "continue"
    end
  end
  struct Error < Expr
    property location : Location
    property message : ::String
    def initialize(@location : Location, @message : ::String)
    end
    def to_s(io : IO)
      io << "Error(#{message})"
    end
  end

  struct Import < IrNode
    property location : Location
    property path : ::String
    property module_alias : ::String?
    property bindings : ::Array({::String, ::String})?

    def initialize(
      @location : Location, 
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

  struct Type < IrNode
    property location : Location
    property name : ::String
    property type_args : Slice(Type)
    property binding : Binding?
    def initialize(@location : Location, @name : ::String, @type_args : Slice(Type) = Slice(Type).empty, @binding : Binding? = nil)
    end
    def initialize(@location : Location, @name : ::String, type_args : ::Array(Type)? = nil, @binding : Binding? = nil)
      @type_args = type_args.try &.to_unsafe_slice || Slice(Type).empty
    end
    def to_s(io : IO)
      io << name
      if (t_args = type_args).any?
        io << "[#{t_args.join(", ")}]"
      end
    end
    # def_eq
    # def_inspect
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{location})"
    # end
  end

  struct If < Expr
    property location : Location
    property conditionals : ::Array(Condition)
    def initialize(@location : Location, @conditionals : ::Array(Condition))
    end
    def branching? : Bool
      true
    end
    def to_s(io : IO)
      io << "if "
      if conditionals.size == 1
        io << conditionals[0]
      else
        io << "\n  #{conditionals.join("\n  ")}\n"
      end
    end
  end

  # represents at least one complete condition
  abstract struct Condition < IrNode
    def self.binary(*args)
      BinaryCondition.new(*args).as(Condition)
    end
    def self.unary(*args)
      UnaryCondition.new(*args).as(Condition)
    end
    def self.else(*args)
      ElseCondition.new(*args).as(Condition)
    end
  end
  struct BinaryCondition < Condition
    property location : Location
    property lhs : Expr
    property op_split : ::Array(OpBranch)
    def initialize(@location, @lhs, @op_split = [] of OpBranch)
    end
    def to_s(io : IO)
      io << lhs
      if op_split.size == 1
        io << ' ' << op_split[0]
      else
        io << "\n    #{op_split.join("\n    ")}"
      end
    end
  end
  alias UnaryCondition = RTerm
  struct ElseCondition < Condition
    property location : Location
    property consequent : Expr
    def initialize(@location, @consequent)
    end
    def to_s(io : IO)
      io << "else: #{consequent}"
    end
  end

  struct OpBranch < IrNode
    property location : Location
    property operator : Operator
    property term_split : ::Array(RTerm)
    def initialize(@location, @operator, @term_split = [] of RTerm)
    end
    def to_s(io : IO)
      io << operator
      if term_split.size == 1
        io << ' ' << term_split[0]
      else
        io << "\n      #{term_split.join("\n      ")}"
      end
    end
  end

  # this struct is used both as the rhs of a binary test, and also as a complete unary test
  struct RTerm < Condition
    property location : Location
    property term : Expr
    @consequent_or_additional_conditions : Expr | ::Array(Condition)
    def initialize(@location, @term, @consequent_or_additional_conditions)
    end

    def consequent
      @consequent_or_additional_conditions.as?(Expr)
    end

    def and
      @consequent_or_additional_conditions.as?(::Array(Condition))
    end
    def to_s(io : IO)
      io << term
      if a = and
        if a.size == 1
          io << " and #{a[0]}"
        else
          io << " and {#{a.join(", ")}}"
        end
      else
        io << ": #{consequent}"
      end
    end
  end
    








  abstract struct TopLevelItem < IrNode
    property location : Location
    # property type_params : Slice(TypeParameter)
    def initialize(@location : Location)
    end
    abstract def type_params : Slice(TypeParameter)
    abstract def to_s(io : IO)
    # def inspect(io : IO)
    #   io << "#{self.class.name}(#{location})"
    # end
  end

  abstract struct TypeDeclaration < TopLevelItem
    property convention : Convention
    property name : ::String
    property type_params : Slice(TypeParameter)
    property traits : ::Array(Type)?
    def initialize(@location : Location, @name : ::String, @type_params : Slice(TypeParameter), @convention : Convention, @traits : ::Array(Type)? = nil)
    end
    def as_extension
      Extend.new(
        location, 
        type_params,
        Type.new(location, name, Ast.to_type_args(type_params), convention.try &.to_binding), 
        traits)
    end
  end

  
  struct Struct < TypeDeclaration
    property fields : ::Array(Field)
    def initialize(@location : Location, @name : ::String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @traits : ::Array(Type)? = nil, @fields : ::Array(Field) = [] of Field, @convention : Convention = nil)
    end
    def initialize(@location : Location, @convention : Convention, @name : ::String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @traits : ::Array(Type)? = nil, @fields : ::Array(Field) = [] of Field)
    end
    def to_s(io : IO)
      io << "struct #{name}"
    end
  end

  struct Field < IrNode
    property location : Location
    property name : ::String
    property type : Type
    property init : Expr?
    def initialize(@location : Location, @name : ::String, @type : Type, @init : Expr? = nil)
    end
    def binding : Binding
      Binding::Var
    end
    def to_s(io : IO)
      io << "field #{name}: #{type}"
      io << " = #{init}" if init
    end
  end

  struct Enum < TypeDeclaration
    property variants : ::Array(Variant)
    def initialize(@location : Location, @name : ::String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @traits : ::Array(Type)? = nil, @variants : ::Array(Variant) = [] of Variant, @convention : Convention = nil)
    end
    def initialize(@location : Location, @convention : Convention, @name : ::String, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @traits : ::Array(Type)? = nil, @variants : ::Array(Variant) = [] of Variant)
    end
    def to_s(io : IO)
      io << "enum #{name}"
    end
  end

  struct Variant < IrNode
    property location : Location
    property name : ::String
    property fields : ::Array({::String, Type})?
    def initialize(@location : Location, @name : ::String, @fields : ::Array({::String, Type})? = nil)
    end
    def to_s(io : IO)
      io << "variant #{name}#{fields}"
    end
  end

  struct Trait < TypeDeclaration
    property methods : ::Array(AbstractMethod)
    def initialize(@location : Location, @name : ::String, type_params : Slice(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @methods : ::Array(AbstractMethod) = [] of AbstractMethod, @convention : Convention = nil)
      @type_params = type_params || Slice[TypeParameter.new(@location, "Self")]
    end
    def initialize(@location : Location, @convention : Convention, @name : ::String, type_params : Slice(TypeParameter)? = nil, @traits : ::Array(Type)? = nil, @methods : ::Array(AbstractMethod) = [] of AbstractMethod)
      @type_params = type_params || Slice[TypeParameter.new(@location, "Self")]
    end
    def to_s(io : IO)
      io << "trait #{name}"
    end
  end

  struct Extend < TopLevelItem
    property type : Type
    property type_params : Slice(TypeParameter)
    property traits : ::Array(Type)
    def initialize(@location : Location, @type_params : Slice(TypeParameter), @type : Type, @traits : ::Array(Type) = [] of Type)
    end
    def initialize(@location : Location, @type : Type, *, @type_params : Slice(TypeParameter) = Slice(TypeParameter).empty, @traits : ::Array(Type) = [] of Type)
    end
    def name 
      @type.name
    end
    def to_s(io : IO)
      io << "extend #{type}"
      if t = traits
        io << " is #{t.join(" & ")}"
      end
    end
  end

  # common methods for ExternalFunction, Function, and AbstractMethod
  module HasSignature
    # property signature : Signature
    def count_type_params
      signature.type_params.try &.size || 0
    end
    def type_params : Slice(TypeParameter)
      signature.type_params
    end
    def count_params
      signature.parameters.try &.size || 0
    end
    def parameters
      signature.parameters
    end
    def return_type
      signature.return_type
    end
    def return_convention
      signature.return_convention
    end
  end

  struct ExternalFunction < TopLevelItem
    property name : ::String
    property signature : Signature
    def initialize(@location : Location, @name : ::String, @signature : Signature)
    end
    def to_s(io : IO)
      io << "extern def #{name}#{signature}"
    end
    include HasSignature
  end

  struct Function < TopLevelItem
    property name : ::String
    property signature : Signature
    property body : Expr
    def initialize(@location : Location, @name : ::String, @signature : Signature, @body : Expr)
    end
    def initialize(@name : ::String, @signature : Signature, @body : Expr)
      @location = @signature.location
    end
    def to_s(io : IO)
      io << "def #{name}#{signature}: #{body}"
    end
    include HasSignature
  end

  struct AbstractMethod < TopLevelItem
    property name : ::String
    property signature : Signature
    property body : Expr?
    def initialize(@location : Location, @name : ::String, @signature : Signature, @body : Expr? = nil)
    end
    def to_s(io : IO)
      io << "abstract def #{name}#{signature}"
      io << ": <body>" if body
    end
    include HasSignature
  end

  struct Signature < IrNode
    property location : Location
    property type_params : Slice(TypeParameter)
    property parameters : ::Array(Parameter)
    property return_type : Type?
    property return_convention : Convention?
    def initialize(@location : Location, type_params : Slice(TypeParameter)? = nil, @parameters : ::Array(Parameter) = [] of Parameter, @return_type : Type? = nil, @return_convention : Convention = nil)
      @type_params = type_params || Slice(TypeParameter).empty
    end
    def to_s(io : IO)
      if tp = type_params
        io << "[#{tp.join(", ")}]"
      end
      if params = parameters
        io << "(#{params.join(", ")})"
      end
      if rt = return_type
        io << " -> "
        if rc = return_convention
          io << "#{rc} "
        end
        io << rt
      end
    end
  end

  struct TypeParameter < IrNode
    property location : Location
    property name : ::String
    property constraints : Constraints
    def initialize(@location : Location, @name : ::String, @constraints : Constraints = Constraints.new)
    end
    def initialize(@location : Location, @name : ::String, include_traits = Slice(Type).empty, exclude_traits = Slice(Type).empty)
      @constraints = Constraints.new(include_traits, exclude_traits)
    end
    def initialize(@location : Location, @name : ::String, include_traits = Array(Type))
      @constraints = Constraints.new(include_traits)
    end
    def to_s(io : IO)
      io << "#{name}"
      io << ": " << constraints.to_s unless constraints.empty?
    end
    def inspect(io : IO)
      io << "#{self.class.name}(#{name}"
      io << ": " << constraints.to_s unless constraints.empty?
      io << ")"
    end
  end

  struct Constraints
    property includes : Slice(Type)
    property excludes : Slice(Type)
    def initialize(@includes : Slice(Type) = Slice(Type).empty, @excludes : Slice(Type) = Slice(Type).empty)
    end
    def initialize(includes : (::Array(Type)? | Slice(Type)) = nil, excludes : (::Array(Type)? | Slice(Type)) = nil)
      @includes = if includes.is_a?(Slice)
        includes
      else
        includes.try &.to_unsafe_slice || Slice(Type).empty
      end
      @excludes = if excludes.is_a?(Slice)
        excludes
      else
        excludes.try &.to_unsafe_slice || Slice(Type).empty
      end
    end
    def self.include(trait : Type)
      new(includes: Slice[trait])
    end
    def self.exclude(trait : Type)
      new(excludes: Slice[trait])
    end
    def +(trait : Type)
      new(@includes.push(trait), @excludes)
    end
    def -(trait : Type)
      new(@includes, @excludes.push(trait))
    end
    def empty?
      includes.empty? && excludes.empty?
    end
    def to_s(io : IO)
      io << includes.join(" & ") 
      if excludes.any?
        io << " ~" << excludes.join(" ~") 
      end
    end
    def inspect(io : IO)
      io << "Ast::Constraints(includes: #{includes.inspect}, excludes: #{excludes.inspect})"
    end
  end    

  struct Parameter < IrNode
    property location : Location
    property name : ::String
    property type : Type
    property convention : Convention
    def initialize(@location : Location, @name : ::String, @type : Type, @convention : Convention = nil)
    end
    def initialize(@location : Location, @convention : Convention, @name : ::String, @type : Type)
    end
    def to_s(io : IO)
      io << "#{convention} " if convention
      io << "#{name}: #{type}"
    end
  end

  def self.to_type_args(type_params : Slice(TypeParameter)) : Slice(Type)
    return Slice(Type).empty unless type_params
    type_params.map { |tp| Type.new(tp.location, tp.name) }
  end
end

