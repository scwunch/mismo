require "./spec_helper"
require "../src/type_checker"
require "../src/type_env"
require "../src/types"
require "../src/parser"
require "../src/lexer"

def parser(
    source : String, 
    level : Logger::Level = Logger::Level::Warning)
  parser(nil, source, level)
end
def parser(
    file : (String | Nil),
    source : String, 
    level : Logger::Level = Logger::Level::Warning)
  logger = Logger.new(level, file_path: file)
  lexer = Lexer.new(
    Lexer::Reader.new(source), 
    if level == Logger::Level::Debug
      Logger.new(Logger::Level::Info)
    else
      logger
    end
  )
  Parser.new(lexer, logger)
end
def expression_parser(test_name : String, code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(parser(test_name, code, level))
end
def expression_parser(code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(
    parser: parser(code, level),
    # expression_indent: 2,
    # line_indent: 2,
    # stop: StopAt::Normal
  )
end

def loc(line, column)
  Location.new(line, column)
end
def loc 
  Location.zero
end

def type_env(level : Logger::Level = Logger::Level::Warning)
  TypeEnv.new(Logger.new(level))
end
def type_checker(level : Logger::Level = Logger::Level::Warning)
  TypeContext.new(type_env(level))
end
def type_check_program(code : String, level : Logger::Level = Logger::Level::Warning)
  type_env = type_env(level)
  type_env.type_check_program(parser(code, level).parse)
end

describe TypeContext do
  describe "#type_check" do
    it "works" do
      type_checker = type_checker()
      ast = expression_parser("nil").parse
      hir = type_checker.type_check(ast)
      type_checker.unify(hir.type, Type.nil).should eq(:ok)
      hir.should eq(Hir::Nil.new(loc))
    end
  end
end

describe TypeEnv do
  describe "#type_check_program" do
    it "works" do
      program = <<-MISMO
        struct Point
          field x Int
          field y Int

          def check_me Int:
            let p = Point.new(1, 2)
            p.x = 3
            p.y = 4
            p.x + p.y
        MISMO
      type_check_program(program)  #, :debug)
    end
  end
end
