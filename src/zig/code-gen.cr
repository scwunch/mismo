require "../type_checker/*"
require "../utils/validate_zig_identifier"

class CodeGenerator
  property io : IO
  property indent : UInt32 = 0
  property current_function : FunctionDef = FunctionDef.new(Location.zero, "<none>")
  def initialize(@io : IO)
  end

  def generate_program(env : TypeEnv)
    env.type_defs.each do |name, type|
      emit(type)
    end
    newline
    env.functions.each do |name, functions|
      functions.each_with_index do |function, i|
        next if function.extern?
        @current_function = function
        emit(function, i)
      end
    end
  end

  def newline
    io << "\n"
    indent.times do
      io << "  "
    end
  end

  def start_block
    io << " {"
    @indent += 1
    newline
  end

  def end_block
    @indent -= 1
    newline
    io << '}'
  end

  def ident(name : String)
    if valid_zig_identifier?(name)
      io << name
    else
      io << "@\"#{name}\""
    end
  end

  def function_name(name : String, overload_index : Int)
    if valid_zig_identifier?(name)
      io << name << "_"
      overload_index.to_s(io)
    else
      io << "@\"#{name}_"
      overload_index.to_s(io)
      io << '"'
    end
  end

  def type_name(name : String)
    if valid_zig_identifier?(name)
      io << name << "_t"
    else
      io << "@\"#{name}_t\""
    end
  end

  def emit(type_def : TypeDefinition)
    case type_def
    when StructDef
      if type_def.type_params.any?
        emit_generic_type_def(type_def)
      else
        emit_struct_def(type_def)
      end
    when EnumDef
      raise "TODO: emit enum"
    else
      raise "unhandled type def: #{type_def}"
    end
  end

  def emit_struct_def(type_def : StructDef)
    io << "pub const "
    type_name(type_def.name)
    io << " = "
    emit_anonymous_struct_def(type_def)
    io << ';'
    newline
  end

  def emit_anonymous_struct_def(type_def : StructDef)
    io << "struct"
    start_block
    join(type_def.fields.each, ->{io << ','; newline}) do |field|
      ident(field.name)
      io << ": "
      emit(field.type)
    end
    end_block
  end

  def emit_generic_type_def(type_def : StructDef)
    # generic types are implemented as comptime functions in Zig
    # they are instantiated as needed
    io << "pub fn "
    type_name(type_def.name)
    type_def.type_params.each_with_index do |param, i|
      io << (i == 0 ? "(" : ", ")
      emit_type_param(i)
    end
    io << ") type"
    start_block
    io << "return "
    emit_anonymous_struct_def(type_def)
    io << ';'
    end_block
    newline
  end

  def join(iter : Iterator, joiner, &block)
    first = iter.next
    return if first.is_a?(Iterator::Stop)
    yield first
    iter.each do |item|
      io << joiner
      yield item
    end
  end

  def join(iter : Iterator, join_proc, &block)
    first = iter.next
    return if first.is_a?(Iterator::Stop)
    yield first
    iter.each do |item|
      join_proc.call
      yield item
    end
  end

  def emit(function : FunctionDef, index : Int)
    io << "pub fn "
    function_name(function.name, index)
    io << "("
    if function.type_params.any?
      emit_type_param(0)
      (function.type_params.size - 1).times do |i|
        io << ", "
        emit_type_param(i)
      end
      if function.parameters.any?
        io << ", "
      end
    end
    if function.parameters.any?
      emit(function.parameters.first)
      function.parameters[1..].each do |param|
        io << ", "
        emit(param)
      end
    end
    io << ") "
    if function.return_type == Type.nil
      io << "void"
    else
      emit(function.return_type)
    end

    # emit body
    start_block
    function.body.each_with_index do |stmt, i|
      if i == function.body.size - 1 && function.return_type != Type.nil
        io << "return " unless stmt.is_a?(Hir::Return)
        emit(stmt)
        io << ";"
      else
        emit(stmt)
        io << ";" unless stmt.is_a?(Hir::Block | Hir::If)
        newline
      end
    end
    if function.return_type == Type.nil
      io << "return;"
      newline
    end
    end_block
    newline
  end

  def emit_type_param(id : Int)
    io << "comptime "
    io << "T"
    id.to_s(io)
    io << ": type"
  end

  def emit(param : Parameter)
    ident(param.name)
    io << ": "
    emit(param.type)
  end

  def emit(type : Type)
    case type
    when Type::Nil
      io << "void"
    when Type::Bool
      io << "bool"
    when Type::Byte
      io << "u8"
    when Type::Nat
      io << "usize"
    when Type::Int
      io << "isize"
    when Type::Float
      io << "f64"
    when Type::Pointer
      io << "*"
      emit(type.element_type)
    when Type::Slice
      io << "[]"
      emit(type.element_type)
    # when Type::String
    #   io << "String_t"
    # when Type::Array
    #   io << "Array_t("
    #   emit(type.element_type)
    #   io << ")"
    when Type::Tuple
      io << "("
      emit(type.types.first)
      type.types[1..].each do |t|
        io << ", "
        emit(t)
      end
      io << ")"
    when Type::Union
      raise "not implemented"
    when Type::Adt
      emit_struct_or_enum(type)
    when Type::Function
      io << "fn(#{type.args.join(", ")}) "
      emit(type.return_type)
    when Type::Var
      io << "T#{type.id}"
    when Type::Never
      io << "noreturn"
    when Type::Unknown
      abort! "unknown type: #{type}"
    else
      raise "unhandled type: #{type}"
    end
  end

  def emit_struct_or_enum(type : Struct | Enum)
    type_name(type.base.name)
    if type.type_args.any?
      io << "("
      emit(type.type_args.first)
      type.type_args[1..].each do |t|
        io << ", "
        emit(t)
      end
      io << ")"
    end
  end

  def emit(block : Hir::Block)
    blk_id = indent
    io << "@\"block$#{blk_id}\": "
    start_block
    block.statements.each_with_index do |stmt, i|
      if i < block.statements.size - 1
        emit(stmt)
        io << ";"
        newline
      else
        io << "break :@\"block$#{blk_id}\" "
        emit(stmt)
        io << ";"
      end
    end
    end_block
  end

  def emit(expr : Hir)
    case expr
    when Hir::Block
      raise "I thought this would be caught by the overloaded definition."
    when Hir::True
      io << "true"
    when Hir::False
      io << "false"
    # when Hir::Nat
    #   io << "@as(usize, #{expr.value})"
    when Hir::Int
      expr.value.to_s(io)
      # io << "@as(isize, #{expr.value})"
    when Hir::Float
      expr.value.to_s(io)
      # io << "@as(f64, #{expr.value})"
    when Hir::String
      io << "string_from_literal(\""
      io << expr.value
      io << "\")"
    when Hir::Identifier
      ident(expr.name)
    when Hir::TempVar
      io << "@\"temp$#{expr.id}\""
    when Hir::Array
      io << "["
      expr.elements.each do |e|
        emit(e)
        io << ", "
      end
      io << "]"
    when Hir::Tuple
      io << ".{"
      expr.elements.each do |e|
        emit(e)
        io << ", "
      end
      io << "}"
    when Hir::NotNode
      io << "!("
      emit(expr.value.value)
      io << ")"
    when Hir::And
      io << '('
      emit(expr.left.value)
      io << ") and ("
      emit(expr.right.value)
      io << ')'
    when Hir::Or
      io << '('
      emit(expr.left.value)
      io << ") or ("
      emit(expr.right.value)
      io << ')'
    when Hir::Call
      emit_call(expr)
    when Hir::Constructor
      emit_constructor(expr)
    when Hir::Assign
      emit_assign(expr.variable.name, expr.value)
    when Hir::AssignField
      emit(expr.object)
      io << '.'
      emit_assign(expr.field.name, expr.value)
    when Hir::AccessField
      io << "&" if expr.mutable?
      emit(expr.object)
      io << '.'
      ident(expr.field.name)
    when Hir::Let
      emit_declare(expr.name, expr.value)
      # io << "const "
      # if val = expr.value
      #   emit_assign(expr.name, val)
      # else
      #   emit_declare(expr.name)
      # end
    when Hir::Var
      emit_declare(expr.name, expr.value)
    when Hir::If
      emit_if(expr)
    else
      raise "unhandled #{expr.class} expr in #{current_function}: #{expr}"
    end
  end

  def emit_assign(name : String, value : Hir)
    ident(name)
    io << " = "
    emit(value)
  end

  def emit_assign(name : String, value : Cell(Hir))
    emit_assign(name, value.value)
  end

  def emit_declare(name : String, value : Cell(Hir)?)
    io << "var "
    ident(name)
    if value
      io << ": "
      emit(value.value.type)
      io << " = undefined; "
      emit_assign(name, value.value) 
    else
      io << " = undefined"
    end
  end

  def emit(cell : Cell)
    emit(cell.value)
  end

  def emit_call(call : Hir::Call)
    # NOTE: '/' is intentionally left off here because integers cannot be divided with this operator
    if call.function.name.in?("+", "*", "-", "<", "<=", "==", "!=", ">", ">=") &&
        call.args.size == 2 && 
        call.args.all? &.type.primitive? &&
        call.args[0].type == call.args[1].type
      emit call.args[0]
      io << ' '
      io << call.function.name
      io << ' '
      emit call.args[1]
    else
      function_name(call.function.name, call.overload_index)
      call.args.each_with_index do |arg, i|
        io << (i == 0 ? "(" : ", ")
        emit(arg)
      end
      io << ")"
    end
  end

  def emit_if(if_node : Hir::If, blk_id = indent)
    io << "@\"block$" << blk_id << "\": "
    start_block
    if_node.tests_and_bindings.each_with_index do |test, i|
      newline if i > 0
      case test
      in Hir::Test
        # io << (i == 0 ? "if (" : " else if (")
        io << "if ("
        emit(test.expr)
        io << ") "
        case cons = test.@consequent_or_additional_conditions
        in Hir
          io << "break :@\"block$" << blk_id << "\" "
          emit(cons)
        in Array(Hir::TestOrBinding)
          emit_if(Hir::If.new(if_node.location, cons), blk_id)
        end
      in Hir::BindTemp
        io << "const "
        emit_assign("temp$#{test.id}", test.value)
      in Hir::Else
        io << "break :@\"block$" << blk_id << "\" "
        emit(test.consequent)
      in Hir::TestOrBinding
        raise "exhaustive case to satisfy crystal compiler"
      end
      io << ';'
    end
    end_block
  end

  def emit_constructor(cons : Hir::Constructor)
    emit(cons.type)
    if cons.args.empty?
      io << "{}"
    else
      case ty = cons.type.base
      when StructDef
        cons.args.each_with_index do |arg, i|
          io << (i==0 ? "{ ." : ", .")
          ident(ty.fields[i].name)
          io << " = "
          emit(arg)
        end
        io << " }"
      when EnumDef
        raise "Enum constructor not implemented"
        cons.args.each_with_index do |arg, i|
          io << (i==0 ? "{ ." : ", .")
          ident(ty.variants[i].name)
          io << " = "
          emit(arg)
        end
        io << " }"
      else
        raise "unhandled type: #{ty}"
      end
    end
  end

  def abort!(message : String)
    io.flush
    raise message
  end
end


BUILTIN_SYMBOLS = Set{
  "print",
  "+.0",
  "*.0",
  "-.0",
  "/.0"
}


# module Zig
#   abstract class Node
#     abstract def to_s(io : IO)
#   end

#   abstract class Expression < Node
#   end

#   class FunctionDef < Node
#     property name : String
#     property params : Array<Param>
#     property return_type : Expression
#     property body : Expression

#     def initialize(@name : String, @params : Array<Param>, @return_type : Expression, @body : Expression)
#     end

#     def to_s(io : IO)
#       io << "fn #{name}() -> #{body.to_s(io)}"
#     end
#   end

#   class Param < Node
#     property name : String
#     property type : Expression

#     def initialize(@name : String, @type : Expression)
#     end

#     def to_s(io : IO)
#       io << name << ": "
#       type.to_s(io)
#     end
#   end

#   class IntegerLiteral < Expression
#     def initialize(@value : Int)
#     end

#     def to_s(io : IO)
#       io << @value.to_s
#     end
#   end

#   class FloatLiteral < Expression
#     def initialize(@value : Float)
#     end

#     def to_s(io : IO)
#       io << @value.to_s
#     end
#   end


# end