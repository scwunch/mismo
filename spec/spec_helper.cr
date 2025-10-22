require "spec"
require "../src/mismo"
require "../src/*"

macro not_nil!(prop)
  {{ prop }}.should_not be_nil
  raise "" unless {{prop}}
end

def parser(source : String, level : Logger::Level = Logger::Level::Warning)
  parser(nil, source, 1,level)
end
def parser(
    file_path : (String | Nil),
    source : String, 
    line_offset,
    level : Logger::Level = Logger::Level::Warning
  ) : Parser
  logger = Logger.new(level, out: TestOut.silent)
  lexer = Lexer.new(
    Lexer::Reader.new(source, file_path, line_offset), 
    if level == Logger::Level::Debug
      Logger.new(Logger::Level::Info)
    else
      logger
    end
  )
  Parser.new(lexer, logger)
end

def expression_parser(test_name : String, line_offset, code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(parser(test_name, code, line_offset, level))
end
def expression_parser(test_name : String, code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(parser(test_name, code, 0, level))
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
    Logger.new(level)
  else
    Logger.new(out: NullOut.new)
  end
  TypeEnv.new(logger)
end

def type_checker(file : String, program : String, level : Logger::Level = Logger::Level::Warning)
  TypeContext.new(type_env(file, program, level))
end

def type_check_program(file_path : String, line_offset, program : String, level : Logger::Level = Logger::Level::Warning)
  prelude_parser = parser("/home/ryan/programming-projects/crystal/mismo/mismo_code_samples/prelude.mismo", get_prelude, 0, Logger::Level::Error)
  items = prelude_parser.parse
  parser = parser(file_path, program, line_offset, Logger::Level::Error)
  items.concat parser.parse
  parser.log.level = level
  type_env = TypeEnv.new(parser.log)
  type_env.type_check_program(items)
  type_env
end

def call_zig(code : String) : String
  tempfile = File.tempfile("zig_code", suffix: ".zig")
  # temp_path = File.tempname("zig_code", ".zig")
  # File.copy(File.join(__DIR__, "../src/zig/prelude.zig"), temp_path)
  # tempfile = File.open(temp_path, "a")
  # tempfile << '\n'
  tempfile << code
  tempfile.close
  call_zig(tempfile)
end

def call_zig(tempfile : File) : String

  process = Process.new("zig", ["run", tempfile.path], output: :pipe, error: :pipe)
  output = process.output.gets_to_end
  error = process.error.gets_to_end
  status = process.wait

  if status.success?
    # only delete file if no error
    File.delete(tempfile.path) if File.exists?(tempfile.path)
    output
  else
    # You might want to handle compiler errors more gracefully
    STDERR.puts "Zig compilation failed:"
    STDERR.puts error
    ""
  end
ensure
  # File.delete(tempfile.path) if File.exists?(tempfile.path)
end


def zig_build_run
  process = Process.new("zig", ["build", "run"], 
    output: :pipe, 
    error: :pipe, 
    chdir: "/home/ryan/programming-projects/crystal/mismo/src/zig"
  )
  output = process.output.gets_to_end
  error = process.error.gets_to_end
  status = process.wait

  if status.success?
    output
  else
    STDERR.puts "Zig compilation failed:"
    STDERR.puts error
    ""
  end
end

def get_prelude
  File.read("/home/ryan/programming-projects/crystal/mismo/mismo_code_samples/prelude.mismo")
end