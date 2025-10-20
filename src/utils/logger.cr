require "../errors/errors"

class Logger
  enum Level
    Debug
    Info
    Warning
    Error
    def color
      case self
      in Level::Debug
        "\033[90m > "
      in Level::Info
        "\033[94m 🛈 "
      in Level::Warning
        "\033[33m ⚠️ "
      in Level::Error
        "\033[31m 🛑"
      end
    end
  end

  getter file_path : Nil
  # getter source_lines : Array(String)
  @indent : UInt32 = 0
  @out : IO = STDOUT
  property level : Level
  property errors = [] of Error

  def initialize(@level : Level = Level::Info, @out : IO = STDOUT, file_path : String? = nil)
    @file_path = nil
    # if file_path && !file_path.ends_with?(':')
    #   @file_path = file_path + ':'
    # else
    #   @file_path = file_path
    # end
    # if file_path && file_path.to_unsafe.address == 0x10000000c
    #   print "\033[31m***LOGGER.FILE_PATH CORRUPTED IN INITIALIZER***\033[0m\n"
    #   @file_path = nil
    # end
  end
  def initialize(@level : Level = Level::Info, @out : IO = STDOUT, file_path : String? = nil)
    # @source_lines = source.split('\n')
  end

  macro def_log(level)
    def {{level.id.downcase}}(loc : Location, msg : String)
      if @level <= Level::{{level}}
        if (fp = @file_path) && fp.to_unsafe.address == 0x10000000c
          print "\033[31m***LOGGER.FILE_PATH CORRUPTED***\033[0m\n"
          @file_path = nil
        end
        # @out.puts "#{"  " * @indent}#{ Level::{{level}}.color } #{loc} #{msg}\033[0m"
        @out << "\033[90m "
        @out << " \u{2502}" * @indent
        @out << Level::{{level}}.color
        @out << @file_path if @file_path
        loc.coord(@out)
        @out << ' ' << msg
        @out << '\n'
        {% if level.id == "Error" %}
          if 0 < loc.line && loc.line < loc.source.size
            @out << ' '
            @out << " \u{2502}" * @indent
            @out << loc.source[loc.line]
            if loc.column != 0
              @out << "\n "
              @out << " \u{2502}" * @indent
              @out << " " * (loc.column - 1)
              @out << "^\n"
            else
              @out << '\n'
            end
          end
        {% end %}
        @out << "\033[0m"
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
  # @[Deprecated("Use `#error(Error)` instead")]
  def_log Error

  def error(err : Error)
    @errors << err
    err.print(
      "\033[90m " + " \u{2502}" * @indent + "\033[31m",
      # @source_lines,
      @out
    )
    @out << "\033[0m"
  end

  # def contains?(str : String)
  #   case test_out = @out
  #   when TestOut
  #     buf = test_out.@buffer
  #     String.new(buf.to_unsafe, buf.size).includes?(str)
  #   else
  #     raise TypeCastError.new("Logger#contains? called on non-TestOut IO")
  #   end
  # end

  def expect(str)
    case test_out = @out
    when TestOut
      test_out.expect(str)
    else
      raise TypeCastError.new("Logger#expect called on non-TestOut IO")
    end
  end

  def expect(errs : Array(String))
    while errs.any?
      if errors.empty?
        errors.should contain(errs.shift)
      else
        errors.first.to_s.should contain(errs.shift)
        errors.shift
      end
    end
    errors.should be_empty
  end

  def check_expectations_empty
    case test_out = @out
    when TestOut
      test_out.@expecting.should be_empty
    else
      raise TypeCastError.new("Logger#check_expectations_empty called on non-TestOut IO")
    end
  end
end

class NullOut < IO
  def read(slice : Bytes)
  end
  def write(slice : Bytes) : Nil
  end
end

class TestOut < IO
  @expecting : Array(String) = [] of String
  @out : IO = STDOUT
  def self.silent : TestOut
    TestOut.new(IO::Null)
  end

  # add a substring to expect to the end of the queue
  def expect(str : String)
    @expecting << str
  end

  def read(slice : Bytes)
    @out.read(slice)
  end

  # if output matches the next expectation in the queue, remove it
  # otherwise, forward to @out
  def write(slice : Bytes) : Nil
    if !@expecting.empty? && String.new(slice.to_unsafe, slice.size).includes?(@expecting.first)
      @expecting.shift
    else
      @out.write(slice)
    end
  end
end

# Define a macro to get a path relative to the project root.
# `__DIR__` is used to find the path of the directory this macro is defined in.
# This assumes the utility file is at a known, fixed location relative to the project root.
macro rel_path(path)
  # Find the project root directory relative to this file.
  # Adjust `../` count if your utility file is in a different location.
  project_root = {{ Path.new(__DIR__).join("../").to_s.stringify }}

  # Replace the project root from the provided path.
  {{ path }}.to_s.gsub({{ project_root.stringify }}, "").lstrip("/")
end
