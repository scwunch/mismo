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

  getter file_path : Nil
  getter source_lines : Array(String)
  @indent : UInt32 = 0
  @out : IO = STDOUT
  property level : Level

  def initialize(@level : Level = Level::Info, @out : IO = STDOUT, file_path : String? = nil, @source_lines = [] of String)
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
  def initialize(@level : Level = Level::Info, @out : IO = STDOUT, file_path : String? = nil, source : String = "")
    @source_lines = source.split('\n')
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
        @out << loc
        @out << ' ' << msg
        @out << '\n'
        {% if level.id == "Error" %}
          if 0 < loc.line && loc.line < @source_lines.size
            @out << ' '
            @out << " \u{2502}" * @indent
            @out << @source_lines[loc.line - 1]
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
  def_log Error

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

  def check_expectations_empty
    case test_out = @out
    when TestOut
      test_out.@expecting.should be_empty
    else
      raise TypeCastError.new("Logger#check_expectations_empty called on non-TestOut IO")
    end
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
