# The tokens module contains the token types, as well as their data: 
#  - Location
#  - Operator
#  - Keyword
#  
require "./operators"


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
  def initialize(tuple : {Int32, Int32})
    @line = tuple[0].to_u32
    @column = tuple[1].to_u32
  end
  def self.new(loc : Location)
    loc
  end

  def self.zero
    Location.new(0, 0)
  end

  def indent
    @column - 1
  rescue
    0u32
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

    def short : ::String
      @data.to_s
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

  # eg Token.number("34")
  def self.{{name.id.downcase}}(data : {{type}})
    {{name}}.new(Location.zero, data)
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
  def self.{{name.id.downcase}}(location : Location = Location.zero)
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
  def short : ::String
    case self
    when BeginFile then "<begin file>"
    when Not then "not"
    when Comma then ","
    when Dot then "."
    when Colon then ":"
    when Semicolon then ";"
    when LParen then "(" 
    when RParen then ")"
    when LBracket then "["
    when RBracket then "]"
    when LBrace then "{" 
    when RBrace then "}"
    when EOF then "<EOF>"
    else
      self.class.name
    end
  end
  
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
  # define_token Assign
  define_token Operator, ::Operator
  define_token Else
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

  def Newline.short : ::String
    "\n" + @data * " "
  end

  def ucs_branch_token?
    self.is_a?(Operator) && self.data.ucs_branch_op?
  end

  alias GroupOpen = LParen | LBracket | LBrace
  alias GroupClose = RParen | RBracket | RBrace
end

enum KeyWord
  Import
  Struct
  Enum
  Extend
  Trait
  Field
  Constructor
  Static
  Def
  Let
  Var
  Mut
  Box
  Ref
  Move
  Const
  If
  # Else
  For
  While
  Break
  Continue
  Return

  def top_level_keyword?
    case self
    when Def, Import, Struct, Enum, Extend, Trait
        true
    else
        false
    end
  end

  # macro parse?(str, ctx)
  #   {% if ctx == :block %}
  #     KeyWord.parse_in_block?({{str}})
  #   {% elsif ctx == :top_level %}
  #     KeyWord.parse_top_level?({{str}})
  #   {% else %}
  #     {% raise "KeyWord.parse? expects :block or :top_level context" %}
  #   {% end %}
  # end

  def self.parse?(str : String, ctx : ParserContext)
    case ctx
    in ParserContext::Block, ParserContext::List
      parse_in_block?(str)
    in ParserContext::TopLevel
      parse_top_level?(str)
    end
  end

  def self.parse?(arg) : Never
    raise "KeyWord.parse? expects 2 arguments: String and one of :block or :top_level context.  (Got `KeyWord.parse?(#{arg})`)"
  end

  def self.parse_top_level?(str : String)
    case str
    when "import" then Import
    when "struct" then Struct
    when "enum" then Enum
    when "extend" then Extend
    when "trait" then Trait
    when "field" then Field
    when "constructor" then Constructor
    when "static" then Static
    when "def" then Def
    when "const" then Const
    else
      nil
    end
  end

  def self.parse_in_block?(str : String)
    case str
    when "let" then Let
    when "var" then Var
    when "mut" then Mut
    when "if" then If
    # when "else" then Else
    when "for" then For
    when "while" then While
    when "break" then Break
    when "continue" then Continue
    when "return" then Return
    else
      nil
    end
  end

  def self.parse?(str)
    case str
    when "let" then Let
    when "var" then Var
    when "mut" then Mut
    when "box" then Box
    when "ref" then Ref
    when "move" then Move
    when "if" then If
    # when "else" then Else
    when "for" then For
    when "while" then While
    when "break" then Break
    when "continue" then Continue
    when "return" then Return
    else
      nil
    end
  end

  def to_s
    case self
    in Import then "import"
    in Struct then "struct"
    in Enum then "enum"
    in Extend then "extend"
    in Trait then "trait"
    in Field then "field"
    in Constructor then "constructor"
    in Static then "static"
    in Def then "def"
    in Let then "let"
    in Var then "var"
    in Mut then "mut"
    in Box then "box"
    in Ref then "ref"
    in Move then "move"
    in Const then "const"
    in If then "if"
    # in Else then "else"
    in For then "for"
    in While then "while"
    in Break then "break"
    in Continue then "continue"
    in Return then "return"
    end
  end  
end