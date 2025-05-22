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

  @indent : UInt32 = 0
  property level : Level

  def initialize(@level : Level = Level::Debug, @out : IO = STDOUT)
  end

  def debug(loc : Location, msg : String)
    if @level <= Level::Debug
      @out.puts "#{loc}: DEBUG: #{msg}"
    end
  end

  def info(loc : Location, msg : String)
    if @level <= Level::Info
      @out.puts "#{loc}: INFO: #{msg}"
    end
  end

  def warning(loc : Location, msg : String)
    if @level <= Level::Warning
      @out.puts "#{loc}: WARNING: #{msg}"
    end
  end

  def error(loc : Location, msg : String)
    if @level <= Level::Error
      @out.puts "#{loc}: ERROR: #{msg}"
    end
  end

  macro def_log(level)
    def {{level.id.downcase}}(loc : Location, msg : String)
      if @level <= Level::{{level}}
        @out.puts "#{"  " * @indent}#{ Level::{{level}}.color } #{loc} #{msg}\033[0m"
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