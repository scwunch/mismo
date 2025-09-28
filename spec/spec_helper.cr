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
  logger = Logger.new(level, file_path: file, out: TestOut.new, source: source)
  lexer = Lexer.new(
    Lexer::Reader.new(source), 
    if level == Logger::Level::Debug
      Logger.new(Logger::Level::Info, source: source)
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

def type_env(file : String, program : String, level = Logger::Level::Warning)
  logger = if level
    Logger.new(level, file_path: file, source: program)
  else
    Logger.new(file_path: file, source: program, out: NullOut.new)
  end
  TypeEnv.new(logger)
end

def type_checker(file : String, program : String, level : Logger::Level = Logger::Level::Warning)
  TypeContext.new(type_env(file, program, level))
end

def type_check_program(file : String, code : String, level : Logger::Level = Logger::Level::Warning)
  parser = parser(file, code, Logger::Level::Error)
  items = parser.parse
  parser.log.level = level
  type_env = TypeEnv.new(parser.log)
  type_env.type_check_program(items)
  type_env
end