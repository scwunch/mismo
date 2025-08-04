require "./type_checker"
require "./parser"
require "./lexer"

def parser(
    source : String, 
    level : Logger::Level = Logger::Level::Warning
    )
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
      Logger.new(Logger::Level::Warning)
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

program = <<-MISMO
        trait Trivial

        trait Marker
          def is_marker Bool: true

        trait Equatable[T Equatable[Self]]
          def ==(other : T) -> Bool

        trait SelfEquatable
          def ==(other Self) -> Bool

        extend Int is Equatable[Int] & SelfEquatable & Marker
          def ==(other : Int) -> Bool:
            self == other   
        MISMO
items = parser(program).parse
type_env = TypeEnv.new(Logger.new(:debug))
type_env.register_types_and_collect_items(items)
type_env.register_functions
type_env.eval_type_params_and_trait_claims(items)
type_env.check_trait_implementations
puts "Ran type_checker.cr"