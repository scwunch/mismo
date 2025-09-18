require "spec"
require "../src/mismo"
require "../src/*"

macro not_nil!(prop)
  {{ prop }}.should_not be_nil
  raise "" unless {{prop}}
end

def parser(source : String, level : Logger::Level = Logger::Level::Warning)
  parser(nil, source, level)
end
def parser(
    file : (String | Nil),
    source : String, 
    level : Logger::Level = Logger::Level::Warning
  ) : Parser
  logger = Logger.new(level, file_path: file, out: TestOut.new)
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
  ExpressionParser.new(parser(code, level))
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
  parser = Parser.new(code)
  items = parser.parse
  parser.log.level = level
  type_env = TypeEnv.new(parser.log)
  type_env.type_check_program(items)
  type_env
end