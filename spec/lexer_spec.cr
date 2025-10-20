require "./spec_helper"
# require "../src/lexer"

LOG_LEVEL = Logger::Level::Warning

describe Lexer::Reader do

  it "works" do
    reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
    reader.peek.should eq('h')
    reader.next.should eq('h')
    reader.peek.should eq('e')
    reader.next.should eq('e')
    reader.peek.should eq('l')
    reader.next.should eq('l')
    reader.peek.should eq('l')
    reader.next.should eq('l')
    reader.peek.should eq('o')
    reader.next.should eq('o')
    reader.peek.should eq('\n')
    reader.next.should eq('\n')
    reader.peek.should eq('w')
    reader.next.should eq('w')
    reader.peek.should eq('o')
    reader.next.should eq('o')
    reader.peek.should eq('r')
    reader.next.should eq('r')
    reader.peek.should eq('l')
    reader.next.should eq('l')
    reader.peek.should eq('d')
    reader.next.should eq('d')
    reader.peek.should eq('\0')
    reader.next.should eq('\0')
  end

  describe "#peek" do
    it "returns the next character" do
      reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
      reader.peek.should eq('h')
    end
  end

  describe "#next" do
    it "returns the next character" do
      reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
      reader.next.should eq('h')
      reader.next.should eq('e')
      reader.next.should eq('l')
      reader.next.should eq('l')
      reader.next.should eq('o')
      reader.next.should eq('\n')
      reader.next.should eq('w')
      reader.next.should eq('o')
      reader.next.should eq('r')
      reader.next.should eq('l')
      reader.next.should eq('d')
      reader.next.should eq('\0')
      reader.next.should eq('\0')
      reader.next.should eq('\0')
    end
  end

  describe "#location" do
    it "returns the current location" do
      reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
      reader.location.should eq(Location.new(1, 1))
      4.times { reader.next }
      reader.peek.should eq('o')
      reader.location.should eq(Location.new(1, 5))
      reader.next.should eq('o')
      reader.location.should eq(Location.new(1, 6))
      reader.next.should eq('\n')
      reader.location.should eq(Location.new(2, 1))
      reader.next.should eq('w')
      reader.location.should eq(Location.new(2, 2))
      reader.next.should eq('o')
      reader.location.should eq(Location.new(2, 3))
      reader.next.should eq('r')
      reader.location.should eq(Location.new(2, 4))
      reader.next.should eq('l')
      reader.location.should eq(Location.new(2, 5))
      reader.next.should eq('d')
      reader.location.should eq(Location.new(2, 6))
    end
    it "returns the location of the reader in multi-line text" do
      lexer = Lexer::Reader.new(
        file_name: __FILE__,
        line_offset: __LINE__ + 2,
        text: <<-MISMO
          function greet(name String):
            print("hello, " + name)
          MISMO
        )
      lexer.location.should eq(Location.new(1, 1))
      until lexer.next == '\n' 
      end
      lexer.location.should eq(Location.new(2, 1))
      lexer.next; lexer.next
      lexer.peek.should eq('p')
      lexer.location.should eq(Location.new(2, 3))
    end
  end

  describe "#peek_str" do
    it "returns the next characters" do
      reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
      reader.peek_str(1).should eq("h")
      reader.peek_str(5).should eq("hello")
      reader.peek_str(6).should eq("hello\n")
      reader.next
      reader.peek_str(5).should eq("ello\n")
    end
  end

  describe "#peek_str?" do
    it "checks to see if the given string matches the substring following the current index of the reader" do
      reader = Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__)
      reader.peek_str?("h").should eq(true)
      reader.peek_str?("hi").should eq(false)
      reader.peek_str?("hello").should eq(true)
      reader.peek_str?("world").should eq(false)
      reader.peek_str?("hello\nworld").should eq(true)
      5.times { reader.next }
      reader.next.should eq('\n')
      reader.peek_str?("world").should eq(true)
      reader.peek_str?("hello\nworld").should eq(false)
    end
  end
end

describe Lexer do
  describe "#push_token" do
    it "stores the given token in @current_token" do
      lexer = Lexer.new(
        Lexer::Reader.new("", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      lexer.current_token.class.should eq(Token::BeginFile)
      lexer.push_token(Token.int({1,1}, 123))
      lexer.current_token.class.should eq(Token::Int)
      lexer.push_token(Token.string({1,1}, "hello"))
      lexer.current_token.class.should eq(Token::String)
      lexer.push_token(Token.variable(Location.zero, "world"))
      lexer.current_token.class.should eq(Token::Variable)
      lexer.push_token(Token.eof({1,1}))
      lexer.current_token.class.should eq(Token::EOF)
    end
  end

  describe "#push_operator" do
    it "reads an operator" do
      lexer = Lexer.new(
        Lexer::Reader.new("+ *= and &", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::Add))
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::MulAssign))
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::And))
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::And))
    end

    it "parses the longest possible operator (up to three characters)" do
      lexer = Lexer.new(
        Lexer::Reader.new("+= *= :=", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::AddAssign))
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::MulAssign))
      lexer.push_operator(loc); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::Assign))
    end
  end

  describe "#push_string" do
    it "reads a string" do
      lexer = Lexer.new(
        Lexer::Reader.new(
          file_name: __FILE__, 
          line_offset: __LINE__ + 2,
          text: <<-MISMO
          "hello" `backticks` 'world'
          MISMO
        ),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.push_string(loc); lexer.reader.next
      lexer.current_token.should eq(Token.string(loc, "hello"))
      lexer.push_string(loc); lexer.reader.next
      lexer.current_token.should eq(Token.string(loc, "backticks"))
      lexer.push_string(loc); lexer.reader.next
      lexer.current_token.should eq(Token.string(loc, "world"))
    end
  end

  describe "#push_number" do
    it "reads a number" do
      lexer = Lexer.new(
        Lexer::Reader.new("123 456.789_012 33_.44.55 66.to_string", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.push_number(loc); lexer.reader.next
      lexer.current_token.should eq(Token.int(loc, 123))
      lexer.push_number(loc); lexer.reader.next
      lexer.current_token.should eq(Token.float(loc, 456.789_012))
      lexer.push_number(loc); lexer.reader.next
      lexer.current_token.should eq(Token.float(loc, 33.44))
      lexer.push_number(loc); lexer.reader.next
      lexer.current_token.should eq(Token.int(loc, 55))
      lexer.push_number(loc); lexer.reader.next
      lexer.current_token.should eq(Token.int(loc, 66))
      lexer.reader.peek_str?("to_string").should eq(true)
    end
  end

  describe "#read_word" do
    it "reads a word" do
      lexer = Lexer.new(
        Lexer::Reader.new("hello\nworld", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.read_word(loc).should eq("hello")
      lexer.reader.next.should eq('\n')
      lexer.read_word(loc).should eq("world")
    end
  end

  describe "#push_word" do
    it "reads a word and pushes an operator, keyword, variable, or type name" do
      lexer = Lexer.new(
        Lexer::Reader.new("import\nstruct\nhello world def and or is not then not in", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      loc = Location.zero
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      # lexer.current_token.should eq(Token.keyword(loc, KeyWord::Import))
      lexer.current_token.should eq(Token.variable(loc, "import"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      # lexer.current_token.should eq(Token.keyword(loc, KeyWord::Struct))
      lexer.current_token.should eq(Token.variable(loc, "struct"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should eq(Token.variable(loc, "hello"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should eq(Token.variable(loc, "world"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      # lexer.current_token.should eq(Token.keyword(loc, KeyWord::Def))
      lexer.current_token.should eq(Token.variable(loc, "def"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::And))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should eq(Token.operator(loc, Operator::Or))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should_not eq(Token.operator(loc, Operator::Is))
      lexer.current_token.should eq(Token.operator(loc, Operator::IsNot))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should eq(Token.variable(loc, "then"))
      lexer.push_word(loc, lexer.read_word(loc)); lexer.reader.next
      lexer.current_token.should_not eq(Token.operator(loc, Operator::Not))
      lexer.current_token.should eq(Token.operator(loc, Operator::NotIn))
    end
  end

  describe "#skip_comment" do
    it "consumes the rest of the line, excluding the newline character" do
      lexer = Lexer.new(
        Lexer::Reader.new("-- comment\nhello\nworld", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      lexer.skip_comment
      lexer.reader.peek.should eq('\n')
    end
  end

  describe "#skip_whitespace_and_comments" do
    it "consumes whitespace characters" do
      lexer = Lexer.new(
        Lexer::Reader.new("\n\t\r\f\v -- this is a comment\n   hello\nworld", file_name: __FILE__, line_offset: __LINE__),
        Logger.new(LOG_LEVEL)
      )
      lexer.skip_whitespace_and_comments
      lexer.reader.peek.should eq('h')
    end
  end

  describe "#push_newline" do
    it "consumes all whitespace and comments until the next non-whitespace character \
        and emits a newline token" do
      lexer = Lexer.new(
        Lexer::Reader.new(
          file_name: __FILE__, 
          line_offset: __LINE__ + 2,
          text: <<-MISMO
          def
          -- comment

            
            hello
          world
          MISMO
        ),
        Logger.new(LOG_LEVEL)
      )
      lexer.reader.next  # d
      lexer.reader.next  # e
      lexer.reader.next  # f
      lexer.push_newline
      lexer.reader.peek.should eq('h')
      lexer.current_token.should eq(Token.newline({1, 4}, 2))
    end
  end

  describe "#next" do
    it "lexes the next token in the input" do
      lexer = Lexer.new(
        Lexer::Reader.new(
          file_name: __FILE__, 
          line_offset: __LINE__ + 2,
          text: <<-MISMO
          function greet(name String):
            print("hello, " + name)
          MISMO
        ),
        Logger.new(LOG_LEVEL)
      )
      [
        Token.variable({1, 1}, "function"),
        Token.variable({1, 10}, "greet"),
        Token.lparen({1, 15}),
        Token.variable({1, 16}, "name"),
        Token.type({1, 21}, "String"),
        Token.rparen({1, 27}),
        Token.colon({1, 28}),
        Token.newline({1, 29}, 2),
        Token.variable({2, 3}, "print"),
        Token.lparen({2, 8}),
        Token.string({2, 9}, "hello, "),
        Token.operator({2, 19}, Operator::Add),
        Token.variable({2, 21}, "name"),
        Token.rparen({2, 25}),
        Token.newline({2, 26}, 0)
      ].each do |token|
        lexer.next.should eq(token)
      end
    end
  end

  describe "#parse_path" do
    it "parses an import path optionally delimited by quotes" do
      lexer = Lexer.new(
        Lexer::Reader.new(
          file_name: __FILE__, 
          line_offset: __LINE__ + 2,
          text: <<-MISMO
          'hello'
          path/to/module
          path\\ with\\ spaces as Alias
          MISMO
        ),
        Logger.new(LOG_LEVEL)
      )
      lexer.parse_path.should eq("hello")
      lexer.reader.next.should eq('\n')
      lexer.parse_path.should eq("path/to/module")
      lexer.reader.next.should eq('\n')
      lexer.parse_path.should eq("path with spaces")
      lexer.reader.next.should eq(' ')
      lexer.reader.peek_str?("as Alias").should be_true
    end
  end

  describe "#parse_import_bindings" do
    it "parses import bindings" do
      lexer = Lexer.new(
        Lexer::Reader.new(
          file_name: __FILE__, 
          line_offset: __LINE__ + 2,
          text: <<-MISMO
          {foo, bar}
          { foo , bar }
          MISMO
        ),
        Logger.new(LOG_LEVEL)
      )
      lexer.parse_import_bindings.should eq([{"foo", "foo"}, {"bar", "bar"}])
      lexer.reader.next.should eq('\n')
      lexer.parse_import_bindings.should eq([{"foo", "foo"}, {"bar", "bar"}])
    end
  end

  # describe "#parse_import" do
    # it "parses a single import given a location and path, with alias and bindings" do
    #   lexer = Lexer.new(
    #     Lexer::Reader.new("as alias {foo, bar}"),
    #     Logger.new(LOG_LEVEL),
    #     
    #   )
    #   import = lexer.parse_import(Location.zero, "module")
    #   import.path.should eq("module")
    #   import.module_alias.should eq("alias")
    #   import.bindings.should eq([{"foo", "foo"}, {"bar", "bar"}])
    # end
  # end

  describe "#handle_import" do
    it "yields one or more import items" do
      # lexer = Lexer.new(
      #   Lexer::Reader.new(<<-MISMO
      #     import std/math
      #     import std/
      #       math as M
      #       big/
      #         int {parse as big_parse}
      #         float {parse as fparse}
      #       http
      #     import src/
      #       utils
      #     MISMO
      #   ),
      #   Logger.new(LOG_LEVEL)
      # )
      # imports.expect([
      #   Ast::Import.new(Location.new(1, 8), "std/math", nil, nil),
      #   Ast::Import.new(Location.new(3, 3), "std/math", "M", nil),
      #   Ast::Import.new(Location.new(5, 5), "std/big/int", nil, [{"parse", "big_parse"}]),
      #   Ast::Import.new(Location.new(6, 5), "std/big/float", nil, [{"parse", "fparse"}]),
      #   Ast::Import.new(Location.new(7, 3), "std/http", nil, nil),
      #   Ast::Import.new(Location.new(9, 3), "src/utils", nil, nil),
      # ])
      # 3.times do 
      #   lexer.read_word.should eq("import")
      #   lexer.reader.next.should eq(' ')
      #   lexer.handle_import
      # end
      # imports.import_idx.should eq(imports.imports.size)
    end
  end
end


# class TestLexerOut
#   include Lexer::Out
#   property tokens = [] of Token
#   property idx = 0
#   property imports = [] of Ast::Import
#   property import_idx = 0

#   def emit(token : Token)
#     token.should eq(@tokens[idx])
#     @idx += 1
#   end

#   def signal_end_of_file
#     puts "EOF"
#   end

#   def send_log(log : Logger)
#     puts log
#   end

#   def expect(token : Token)
#     @tokens << token
#   end

#   def expect(tokens : Array(Token))
#     @tokens.concat(tokens)
#   end

#   def expect(imports : Array(Ast::Import))
#     @imports.concat(imports)
#   end

#   def import(import_loc : Location, path : String, import_alias : String?, import_bindings : Array({String, String})?)
#     import = Ast::Import.new(import_loc, path, import_alias, import_bindings)
#     import.should eq(@imports[@import_idx])
#     @import_idx += 1
#   end
# end

# struct NullLexerOut
#   include Lexer::Out
#   def emit(token : Token)
#   end

#   def signal_end_of_file
#   end

#   def send_log(log : Logger)
#   end

#   def import(import_loc : Location, path : String, import_alias : String?, import_bindings : Array({String, String})?)
#   end
# end