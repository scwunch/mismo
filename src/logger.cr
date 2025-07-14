class Logger
  enum Level
    Debug
    Info
    Warning
    Error
    def color
      case self
      when Level::Debug
        "\033[90m > "
      when Level::Info
        "\033[94m 🛈 "
      when Level::Warning
        "\033[33m ⚠️ "
      when Level::Error
        "\033[31m 🛑"
      end
    end
  end

  getter file_path : String = ""
  @indent : UInt32 = 0
  @out : IO = STDOUT
  property level : Level

  def initialize(@level : Level = Level::Info, @out : IO = STDOUT, file_path : (String | Nil) = nil)
    if file_path && !file_path.ends_with?(':')
      @file_path = file_path + ':'
    else
      @file_path = file_path || ""
    end
  end

  macro def_log(level)
    def {{level.id.downcase}}(loc : Location, msg : String)
      if @level <= Level::{{level}}
        # @out.puts "#{"  " * @indent}#{ Level::{{level}}.color } #{loc} #{msg}\033[0m"
        @out << "\033[90m "
        @out << " \u{2502}" * @indent
        @out << Level::{{level}}.color
        @out << file_path << loc
        @out << ' ' << msg << "\033[0m"
        @out << '\n'
      end
    end

    def {{level.id.downcase}}_descend
      @indent += 1 if @level <= Level::{{level}}
    end

    def {{level.id.downcase}}_ascend
      @indent -= 1 if @level <= Level::{{level}}
    end

    def {{level.id.downcase}}_descend(&block)
      {{level.id.downcase}}_descend
      yield
    ensure
      {{level.id.downcase}}_ascend
    end

    def {{level.id.downcase}}_descend(loc : Location, msg : String, &block)
      {{level.id.downcase}}(loc, msg)
      {{level.id.downcase}}_descend
      yield
    ensure
      {{level.id.downcase}}_ascend
    end
  end

  def_log Debug
  def_log Info
  def_log Warning
  def_log Error
end

# """
# │ > 13:3 eval_type_params_and_trait_claims == (context=TypeContext(type_args: T0, scope: #<TypeScope:0x7b3faf34c360>))
# 🛈 > :  Check trait implementations (1)
# │ > 12:1 Assume Int implements 
# """