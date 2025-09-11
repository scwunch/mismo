require "../type_checker/hir_nodes"
require "../ast/ast_nodes"
require "../type_checker/types"
require "../type_checker/type_checker"
require "../type_checker/type_env"

class Interpreter
  property types = {} of String => TypeInfo
  property functions = {} of String => Array(FunctionBase)
  property stack = [] of StackFrame
  property log : Logger
  def frame
    @stack.last
  end

  def initialize(@log : Logger)
  end
  def initialize(@types = {} of String => TypeInfo, @functions = {} of String => Array(FunctionBase), @log = Logger.new)
  end
  def initialize(program : String, level : Logger::Level = Logger::Level::Warning)
    parser = Parser.new(program)
    items = parser.parse
    parser.log.level = level
    type_env = TypeEnv.new(parser.log)
    type_env.type_check_program(items)
    @types = type_env.user_types
    @functions = type_env.functions
    @log = parser.log
  end

  def self.run(program : String, log_level : Logger::Level = Logger::Level::Warning)
    interpreter = Interpreter.new(program, log_level)
    interpreter.run
  end

  def run
    call(functions["main"][0])
  end

  def call(func : FunctionBase, type_args : Slice(Type), args : Array(Val)) : Val
    frame = StackFrame.new(func, type_args, args)
    stack << frame
    result = Val.nil
    func.body.each do |hir|
      log.info_descend(hir.location, "statement #{hir}") do
        result = eval(hir)
        log.debug(hir.location, " => #{result}")
      end
    end
    stack.pop
    result
  end

  def call(func : FunctionBase, args : Array(Val) = [] of Val) : Val
    call(func, Slice(Type).empty, args)
  end

  def eval(hir : Hir) : Val
    case hir
    when Hir::Nil
      Val.nil
    when Hir::False
      Val.false
    when Hir::True
      Val.true
    when Hir::Int
      Val.new(hir.value.to_i32)
    when Hir::Float
      Val.new(hir.value.to_f64)
    when Hir::String
      Val.new(hir.value)
    when Hir::Identifier
      frame.variables[hir.name]
    when Hir::Assign
      frame.variables.update(hir.variable.name) { eval(hir.value) }
    when Hir::Let, Hir::Var
      if val = hir.value
        frame.variables[hir.name] = eval(val)
      end
      Val.nil
    when Hir::TempVar
      frame.temps[hir.id]
    when Hir::Tuple
      Val.new(Slice.new(hir.elements.size) do |i|
        eval(hir.elements[i])
      end)
    when Hir::Array
      buffer = Pointer(Val).malloc(hir.elements.size)
      hir.elements.each_with_index do |e, i|
        buffer[i] = eval(e)
      end
      Val.new(Slice(Val).new(3) do |i|
        Val.new(case i
        when 0
          hir.elements.size
        when 1
          hir.elements.size
        when 2
          buffer.address
        end)
      end)
      Val.new(Slice[
        Val.new(hir.elements.size), 
        Val.new(hir.elements.size), 
        Val.new(buffer.address)
      ])
    when Hir::Call
      args = hir.args.map { |arg| eval(arg) }
      log.debug_descend(hir.location, "call #{hir.function.name} with args #{args}") do
        call(hir.function, args)
      end
    when Hir::Native
      hir.thunk.call(self)
    when Hir::If
      try_conditionals(hir.tests_and_bindings) || Val.nil
    else
      log.error(hir.location, "unhandled hir type: #{hir.inspect}")
      Val.nil
    end
  end

  def eval(hir_cell : Cell(Hir)) : Val
    eval(hir_cell.value)
  end

  def try_conditionals(conditionals : Array(Hir::TestOrBinding)) : Val?
    conditionals.each do |test_or_binding|
      case test_or_binding
      when Hir::Test
        if eval(test_or_binding.expr).data.as(Bool)
          case cons = test_or_binding.@consequent_or_additional_conditions
          in Hir
            return eval(cons)
          in Array(Hir::TestOrBinding)
            if result = try_conditionals(cons)
              return result
            end
          end
        end
      when Hir::BindTemp
        while frame.temps.size <= test_or_binding.id
          frame.temps << Val.nil
        end
        frame.temps[test_or_binding.id] = eval(test_or_binding.value)
      when Hir::MatchBlock
        val = eval(test_or_binding.value)
        discriminant = val.data.as(Slice(Val)).first.data.as(Int32)
        cons = test_or_binding.jump_table[discriminant]?
        return eval(cons) if cons
        cons = test_or_binding.jump_table.last?
        return eval(cons) if cons
      when Hir::Else
        return eval(test_or_binding.consequent)
      end
    end
    nil
  end

  # alias Value = UInt32 | Slice(Value)

  # def Value.nil : Value
  #   Slice(UInt32).empty
  # end

  # def Value.false : Value
  #   0
  # end

  # def Value.true : Value
  #   1
  # end
end


# struct Slice(T)
#   def self.nil
#     Slice(T).empty
#   end

#   def self.int(value : Int32)
#     Slice(Int32).new(1) { value }.to_unsafe_bytes
#   end

#   def self.float(value : Float64)
#     Slice(Float64).new(1) { value }.to_unsafe_bytes
#   end

#   def as_int : Int32
#     unsafe_slice_of(Int32)[0]
#   end

#   def as_float : Float64
#     unsafe_slice_of(Float64)[0]
#   end
# end

# alias Val = Nil | Bool | Int32 | Float64 | String | UInt64 | Slice(Val)

struct Val
  property data : Nil | Bool | Int32 | Float64 | String | UInt64 | Slice(Val)
  def initialize(@data)
  end
  def self.nil
    Val.new(nil)
  end
  def self.false
    Val.new(false)
  end
  def self.true
    Val.new(true)
  end
  def <=>(other : Val)
    if @data == other.data
      0
    else
      -1
    end
  end
end

struct Record
  property fields : Slice(Val)
  def initialize(@fields : Slice(Val))
  end
end

# struct Value
#   property data : Slice(Value)
#   def initialize(@data : Slice(Value))
#   end
#   def self.nil
#     Value.new(Slice(Value).empty)
#   end
#   def self.false
#     Value.new(Slice)
# end

class StackFrame
  property function : FunctionBase
  property generics : Slice(Type)
  property variables : Hash(String, Val) = {} of String => Val
  property temps : Array(Val) = [] of Val

  def initialize(@function : FunctionBase, @generics : Slice(Type), @variables : Hash(String, Val))
  end
  def initialize(@function : FunctionBase, @generics : Slice(Type), vars : Array(Val))
    vars.each_with_index do |var, i|
      @variables[@function.parameters[i].name] = var
    end
  end
end

struct Hir::Native < Hir
  property location : Location
  property thunk : Proc(Interpreter, Val)
  property type : Type
  def initialize(@location : Location, @thunk : Proc(Interpreter, Val), @type : Type)
  end
  def initialize(@type, @thunk)
    @location = Location.zero
  end
  def_init
  def binding : Binding ; Binding::Var end
  def to_s(io : IO)
    io << "native(#{thunk})"
  end
end