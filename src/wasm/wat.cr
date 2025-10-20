LINE_LIMIT = 79

alias Strings = String | Array(Strings)

class Wat
  @data : String | Array(Wat)
  getter size : Int32  # the length of the string if node is printed inline
  def initialize(@data : String)
    @size = @data.size
  end
  def initialize(data : Array(Wat))
    @data = data
    @size = data.sum { |node| node.size }
    if data.size < 2
      @size += 2
    else
      @size += 1 + data.size
    end
  end
  def self.raw(tree : Strings) : Wat
    case tree
    when String
      Wat.new(tree)
    when Array
      Wat.new(tree.map { |s| Wat.raw(s) })
    else
      raise tree.class.to_s
    end
  end

  def to_s(io : IO)
    case d = @data
    when String
      io << d
    when Array(Wat)
      io << '('
      unless d.empty?
        d.first.to_s(io)
        d[1..].each do |node|
          io << ' '
          node.to_s(io)
        end
      end
      io << ')'
    end
  end

  def fmt(formatter : Formatter)
    case d = @data
    when String
      # p "#fmt called on string data: #{d}"
      formatter.write(d)
    when Array(Wat)
      # p "#fmt called on array of Wat..."
      if @size < LINE_LIMIT || d.size < 2
        to_s(formatter.@io)
      else 
        formatter.write("(")
        d.first.fmt(formatter)
        formatter.indent
        d[1..-2].each do |node|
          node.fmt(formatter)
          formatter.newline
        end
        d.last.fmt(formatter)
        formatter.dedent
        formatter.newline
        formatter.write(")")
      end
    end
  end

  class Formatter
    @indent : Int32 = 0
    @io : IO
    def initialize(@io : IO)
    end
    def indent
      @indent += 2
      newline
    end
    def dedent
      @indent -= 2
    end
    def newline
      @io << '\n'
      @io << " " * @indent
    end
    def println(str : String)
      @io << " " * @indent << str << '\n'
    end
    def write(str : String)
      @io << str
    end
  
    def fmt(node : Wat)
      node.fmt(self)
    end
  end
end

