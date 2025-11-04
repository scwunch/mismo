require "../src/**"

# the compiler manages the entire compilation process
class Compiler
  property log : Logger = Logger.new
  property files : Array(Mismo::File) = [] of Mismo::File
  property target_stage : Stage = Stage::Run
  property zig_dir : Path = Path["./src/zig"]

  def initialize(@log, @files, @target_stage, @zig_dir)
  end
  def initialize(
    log_level = Logger::Level::Info, 
    log_out = STDOUT, 
    @target_stage = Stage::Run, 
    prelude_path : String? = "./mismo_code_samples/prelude.mismo",
    @zig_dir = Path["./src/zig"],

  )
    @log = Logger.new(log_level, log_out)
    if prelude_path
      files << Mismo::File.new(prelude_path)
    end
  end

  def self.config(
    file_path : String,
    input : IO | String | Nil = nil, 
    target_stage : Stage = Stage::Run,
    log_level : Logger::Level = Logger::Level::Info,
    log_out : IO = STDOUT,
    starting_line = 1,
    prelude_path : String? = "./mismo_code_samples/prelude.mismo",
    zig_dir : String = "./src/zig",
  ) 
    files = [] of Mismo::File
    if prelude_path
      files << Mismo::File.new(prelude_path)
    end
    files << Mismo::File.new(file_path, input, starting_line, log_level, log_out)
    Compiler.new(
      Logger.new(log_level, log_out), 
      files, 
      target_stage, 
      Path[zig_dir]
    )
  end

  def add_file(
    file_path : String, 
    input : IO | String | Nil = nil,
    log_level : Logger::Level = Logger::Level::Info,
    log_out : IO = STDOUT,
    starting_line = 1,
  ) : Compiler
    files << Mismo::File.new(file_path, input, starting_line, log_level, log_out)
    self
  end

  enum Stage
    Lexer
    Parser
    DeclarationCollection
    TypeCheck
    CodeGen
    Build
    Run
  end

  def self.run(*args)
    config(*args).run
  end

  def run
    raise ArgumentError.new("No files to compile") if files.empty?
    if target_stage == Stage::Lexer && files.size > 1
      raise ArgumentError.new("Cannot compile multiple files in Lexer stage")
    end
    items = [] of Ast::TopLevelItem

    files.each do |file|
      # Lexer
      file_log = if file.log_level || file.log_out
        Logger.new(file.log_level || log.level, out: file.log_out || log.@out)
      else
        log
      end
      lexer = Lexer.new(
        Lexer::Reader.new(file.input, file.path, file.starting_line), 
        file_log)
      tokens = lexer.lex
      return tokens if target_stage == Stage::Lexer

      # Parser
      parser = Parser.new(tokens, file_log, items)
      parser.parse
      return items if target_stage == Stage::Parser
    end
    
    # TypeEnv
    type_env = TypeEnv.new(log)
    type_env.declaration_collection(items)
    return type_env if target_stage == Stage::DeclarationCollection
    
    # Specialization
    functions = type_env.type_check_functions
    return functions if target_stage == Stage::TypeCheck

    # CodeGenerator
    main_path = zig_dir / "main.zig"
    File.copy(src: zig_dir / "prelude.zig", dst: main_path)
    main_file = File.open(main_path, "a")
    main_file << '\n'
    code_generator = CodeGenerator.new(main_file)
    code_generator.generate_program(type_env, functions)
    main_file.close
    return code_generator if target_stage == Stage::CodeGen
    
    # Build
    process = Process.new("zig", 
      target_stage == Stage::Build ? ["build"] : ["build", "run"], 
      output: :pipe, 
      error: :pipe, 
      chdir: zig_dir
    )
    output = process.output.gets_to_end
    error = process.error.gets_to_end
    status = process.wait

    if status.success?
      output
    else
      STDERR.puts "Zig compilation failed:"
      STDERR.puts error
    end
  end
end

struct Mismo::File
  property path : String
  property input : IO
  property starting_line : Int32
  property log_level : Logger::Level?
  property log_out : IO?

  def initialize(@path, input : String | IO | Nil = nil, @starting_line = 1, @log_level = nil, @log_out = nil)
    case input
    in IO
      @input = input
    in String
      @input = IO::Memory.new(input)
    in Nil
      @input = ::File.open(path)
    end
  end
end