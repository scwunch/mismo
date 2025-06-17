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