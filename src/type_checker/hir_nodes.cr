require "../ast/mode_convention"
require "../ast/tokens"
require "../type_checker/types"
require "../utils/cell"
require "../ast/abstract_node"

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

  struct TempVar < Hir
    property location : Location
    property id : Int32
    property binding : Binding
    property type : Type
    def initialize(@location : Location, @id : Int32, @binding : Binding, @type : Type)
    end
    def to_s(io : IO)
      io << "temp_#{id}"
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
    # def type : Type
    #   function.return_type.substitute(type_args)
    # end
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

  struct Block < Hir
    property location : Location
    property statements : ::Array(Hir)
    def initialize(@location : Location, @statements : ::Array(Hir))
    end
    def binding : Binding ; Binding::Var end
    def type : Type
      statements.last.type
    rescue
      Type.nil
    end
    def to_s(io : IO)
      io << "Block:{#{statements.join(", ")}}"
    end
  end

  # struct If < Hir
  #   property location : Location
  #   property conditions : ::Array(Condition)
  #   def initialize(@location : Location, @conditions : ::Array(Condition))
  #   end
  #   def binding : Binding ; Binding::Var end
  #   def type : Type 
  #     conditions[0].type
  #   end
  #   def to_s(io : IO)
  #     io << "if "
  #     if conditions.size == 1
  #       io << conditions[0]
  #     else
  #       io << "\n  #{conditions.join("\n  ")}\n"
  #     end
  #   end
  # end

  # # represents at least one complete condition
  # abstract struct Condition < IrNode
  #   abstract def type : Type
  #   def self.binary(*args)
  #     BinaryCondition.new(*args).as(Condition)
  #   end
  #   def self.unary(*args)
  #     UnaryCondition.new(*args).as(Condition)
  #   end
  #   def self.else(*args)
  #     ElseCondition.new(*args).as(Condition)
  #   end
  # end
  # struct BinaryCondition < Condition
  #   property location : Location
  #   property lhs : Hir
  #   property op_split : ::Array(OpBranch)
  #   def initialize(@location, @lhs, @op_split)
  #   end
  #   def type : Type
  #     op_split[0].type
  #   end
  #   def to_s(io : IO)
  #     io << lhs
  #     if op_split.size == 1
  #       io << ' ' << op_split[0]
  #     else
  #       io << "\n    #{op_split.join("\n    ")}"
  #     end
  #   end
  # end
  # alias UnaryCondition = RTerm
  # struct ElseCondition < Condition
  #   property location : Location
  #   property consequent : Hir
  #   def initialize(@location, @consequent)
  #   end
  #   def type : Type
  #     consequent.type
  #   end
  #   def to_s(io : IO)
  #     io << "else: #{consequent}"
  #   end
  # end

  # struct OpBranch < IrNode
  #   property location : Location
  #   property operator : Operator
  #   property term_split : ::Array(RTerm)
  #   def initialize(@location, @operator, @term_split)
  #   end
  #   def type : Type
  #     term_split[0].type
  #   end
  #   def to_s(io : IO)
  #     io << operator
  #     if term_split.size == 1
  #       io << ' ' << term_split[0]
  #     else
  #       io << "\n      #{term_split.join("\n      ")}"
  #     end
  #   end
  # end

  # # this struct is used both as the rhs of a binary test, and also as a complete unary test
  # struct RTerm < Condition
  #   property location : Location
  #   property term : Hir
  #   @consequent_or_additional_conditions : Hir | ::Array(Condition)
  #   def initialize(@location, @term, @consequent_or_additional_conditions)
  #   end

  #   def consequent
  #     @consequent_or_additional_conditions.as?(Hir)
  #   end

  #   def and
  #     @consequent_or_additional_conditions.as?(::Array(Condition))
  #   end

  #   def type : Type
  #     case cons = consequent
  #     in Hir
  #       cons.type
  #     in ::Array(Condition)
  #       cons[0].type
  #     end
  #   end

  #   def to_s(io : IO)
  #     io << term
  #     if a = and
  #       if a.size == 1
  #         io << " and #{a[0]}"
  #       else
  #         io << " and {#{a.join(", ")}}"
  #       end
  #     else
  #       io << ": #{consequent}"
  #     end
  #   end
  # end

  struct If < Hir
    property location : Location
    property tests_and_bindings : ::Array(TestOrBinding)
    def initialize(@location, @tests_and_bindings)
    end
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.nil end
    def to_s(io : IO)
      io << "if "
      if tests_and_bindings.size == 1
        io << tests_and_bindings[0]
      else
        io << "\n  #{tests_and_bindings.join("\n  ")}\n"
      end
    end
  end

  abstract struct TestOrBinding < IrNode
    abstract def location : Location
    abstract def type : Type
  end

  struct Test < TestOrBinding
    property expr : Hir
    @consequent_or_additional_conditions : Hir | ::Array(TestOrBinding)
    def initialize(@expr, @consequent_or_additional_conditions)
    end

    def location : Location
      expr.location
    end
    
    def consequent
      @consequent_or_additional_conditions.as?(Hir)
    end

    def and
      @consequent_or_additional_conditions.as?(::Array(TestOrBinding))
    end

    def type : Type
      case cons = consequent
      in Hir
        cons.type
      in ::Array(TestOrBinding)
        Type.union(
          cons.each.filter { |t| !t.is_a?(BindTemp) }.map &.type
        )
      end
    end

    def to_s(io : IO)
      io << expr
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

  struct BindTemp < TestOrBinding
    property value : Hir
    property id : Int32
    def initialize(@value, @id)
    end
    def location : Location ; value.location end
    def type : Type ; value.type end
    def to_s(io : IO)
      io << "$temp_#{id} = #{value}"
    end
  end

  struct MatchExpr < Hir
    property location : Location
    property value : Cell(Hir)
    property discriminant : Int32
    def initialize(@location, @value, @discriminant)
    end
    def initialize(@location, value : Hir, @discriminant)
      @value = Cell.new(value.as(Hir))
    end
    def binding : Binding ; Binding::Var end
    def type : Type ; Type.bool end
    def to_s(io : IO)
      io << "#{value} -> #{discriminant}"
    end
  end

  struct MatchBlock < TestOrBinding
    property location : Location
    property value : Hir
    property jump_table : ::Array(Hir?)
    def initialize(@location, @value, @jump_table)
    end
    def location : Location ; value.location end
    def type : Type
      Type.union(
        jump_table.each.filter { |t| !t.nil? }.map &.type
      )
    end
    def to_s(io : IO)
      io << "#{value} -> #{jump_table.join(", ")}" 
    end
  end
  
  struct Else < TestOrBinding
    property location : Location
    property consequent : Hir
    def initialize(@location, @consequent)
    end
    def type : Type
      consequent.type
    end
    def to_s(io : IO)
      io << "else: #{consequent}"
    end
  end
end