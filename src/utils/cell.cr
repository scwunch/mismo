class Cell(T)
    property value : T
    def initialize(@value : T)
    end
    def ==(other : Cell(T))
      value == other.value
    end
    def ==(other : T)
      value == other
    end
    def inspect(io : IO)
      io << "("
      value.inspect(io)
      io << ")"
    end
    def to_s(io : IO)
      io << "("
      value.to_s(io)
      io << ")"
    end
    def_hash @value
end

# class Object
#   def ==(other : Cell(T))
#     value == other.value
#   end
# end

# extend[T] T
#   def ==(other: Cell[T])
#     value == other.value
    

# def ==[T](self T, other Cell[T]):
#   self == other.value


struct Slice(T)
  def push!(value)
    @pointer = @pointer.realloc(@size + 1)
    @pointer[@size] = value
    @size += 1
    self
  end
  def push(value)
    ptr = Pointer(T).malloc(@size + 1)
    copy = self.class.new(ptr, @size + 1)
    copy.copy_from(self)
    copy[@size] = value
    copy
  end
end

macro push!(slice, value)
  ptr = {{slice}}.@pointer.realloc({{slice}}.@size + 1)
  ptr[{{slice}}.@size] = {{value}}
  {{slice}} = Slice.new(ptr, {{slice}}.@size + 1)
end

# macro push!(slice, value)
#   {{slice}}.@pointer = {{slice}}.@pointer.realloc({{slice}}.@size + 1)
#   {{slice}}.@pointer[{{slice}}.@size] = {{value}}
#   {{slice}}.@size += 1
# end

class Array(T)
  # publicize the #to_unsafe_slice private defs
  def to_unsafe_slice
    Slice.new(@buffer, size)
  end

  def to_unsafe_slice(start : Int, count : Int)
    start, count = normalize_start_and_count(start, count)
    Slice.new(@buffer + start, count)
  end
end