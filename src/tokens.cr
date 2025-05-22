# The tokens module contains the token types, as well as their data: 
#  - Location
#  - Operator
#  - Keyword
#  

struct Location
  include Comparable(Location)
  getter line : UInt32
  getter column : UInt32

  def initialize(@line : UInt32, @column : UInt32)
  end
  def initialize(line, column)
    @line = line.to_u32
    @column = column.to_u32
  end

  def self.zero
    Location.new(0, 0)
  end

  def to_s(io : IO)
    if @column == 0
      if @line == 0
        io << " : "
      else
        io << @line << ": "
      end
    else
      io << @line << ":" << @column
    end
  end

  def <=>(other : Location)
    if @line < other.line
      -1
    elsif @line > other.line
      1
    else
      @column <=> other.column
    end
  end

  def -(int)
    Location.new(@line, @column - int)
  end

  def +(int)
    Location.new(@line, @column + int)
  end
end

macro define_token(name, type)
  struct {{name}} < Token
    getter data : {{type}}

    def initialize(@location : Location, @data : {{type}})
    end
    
    def to_s(io : IO)
      io << "Token::{{name}}(" << @data << ")"
    end

    def inspect(io : IO)
      io << "Token::{{name}}(#{@location}, #{@data})"
    end
  end

  # eg Token.number(Location.new(1, 1), "34")
  def self.{{name.id.downcase}}(location : Location, data : {{type}})
    {{name}}.new(location, data)
  end

  # eg Token.number({1, 1}, "34")
  def self.{{name.id.downcase}}(location : {Int32, Int32}, data : {{type}})
    {{name}}.new(Location.new(location[0].to_u32, location[1].to_u32), data)
  end 
end

macro define_token(name)
  struct {{name}} < Token
    def to_s(io : IO)
      io << "Token::{{name}}"
    end
    
    def inspect(io : IO)
      io << "Token::{{name}}(#{@location})"
    end
  end

  # eg Token.comma(Location.new(1, 1))
  def self.{{name.id.downcase}}(location : Location)
    {{name}}.new(location)
  end

  # eg Token.comma({1, 1})
  def self.{{name.id.downcase}}(location : {Int32, Int32})
    {{name}}.new(Location.new(location[0].to_u32, location[1].to_u32))
  end
end

abstract struct Token
  getter location : Location
  abstract def to_s(io : IO)
  abstract def inspect(io : IO)
  
  def initialize(@location : Location)
  end
  
  def data
    nil
  end

  define_token BeginFile
  # define_token Number, ::String
  define_token Int, ::Int128
  define_token Float, ::Float64
  define_token String, ::String
  define_token Variable, ::String
  define_token Type, ::String
  define_token KeyWord, ::KeyWord
  define_token Assign
  define_token Operator, ::Operator
  define_token Not
  define_token Comma
  define_token Dot
  define_token Colon
  define_token Semicolon
  define_token LParen
  define_token RParen
  define_token LBracket
  define_token RBracket
  define_token LBrace
  define_token RBrace
  define_token Newline, ::UInt32
  define_token Error, ::String
  define_token EOF

end


enum Operator
  Neg
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  Not
  And
  Or
  Is
  IsNot
  In
  NotIn
  Eq
  Neq
  Lt
  Le
  Gt
  Ge
  Assign
  AddAssign
  SubAssign
  MulAssign
  DivAssign
  ModAssign
  AndAssign
  OrAssign
  ExpAssign

  def self.parse?(str : String)
    case str
    when "+" then Add
    when "-" then Sub
    when "*" then Mul
    when "/" then Div
    when "%" then Mod
    when "^", "**" then Exp
    when "not" then Not
    when "&", "and" then And
    when "|", "or" then Or
    when "is" then Is
    when "is not" then IsNot
    when "in" then In
    when "not in" then NotIn
    when "==" then Eq
    when "!=" then Neq
    when "<" then Lt
    when "<=" then Le
    when ">" then Gt
    when ">=" then Ge
    when "=" then Assign
    when "+=" then AddAssign
    when "-=" then SubAssign
    when "*=" then MulAssign
    when "/=" then DivAssign
    when "%=" then ModAssign
    when "&&=" then AndAssign
    when "||=" then OrAssign
    when "**=", "^=" then ExpAssign
    else
      nil
    end
  end
end

enum KeyWord
  Import
  Struct
  Enum
  Function
  Extend
  Trait
  Def
  Let
  Var
  If
  Else

  def top_level_keyword?
    case self
    when Def, Function, Import, Struct, Enum, Extend, Trait
        true
    else
        false
    end
  end

  def self.parse?(str : String)
    case str
    when "import" then Import
    when "struct" then Struct
    when "enum" then Enum
    when "function" then Function
    when "extend" then Extend
    when "trait" then Trait
    when "def" then Def
    when "let" then Let
    when "var" then Var
    when "if" then If
    when "else" then Else
    else
      nil
    end
  end

  def to_s(io : IO)
    io << case self
    in Import then "import"
    in Struct then "struct"
    in Enum then "enum"
    in Function then "function"
    in Extend then "extend"
    in Trait then "trait"
    in Def then "def"
    in Let then "let"
    in Var then "var"
    in If then "if"
    in Else then "else"
    end
  end
end
