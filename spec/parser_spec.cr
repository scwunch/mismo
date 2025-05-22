require "./spec_helper"
require "../src/parser"
require "../src/lexer"

def parser(source : String)
  log = Logger.new(Logger::Level::Info)
  toks = TokenCollector.new
  Lexer.new(source, log, toks).tokenize
  log.level = Logger::Level::Debug
  Parser.new(toks.tokens, log)
end

def loc(line, column)
  Location.new(line, column)
end
def loc 
  Location.zero
end

describe Parser do
  describe "#consume?" do
    it "consumes the current token of the specified type, otherwise nil" do
      parser = Parser.new(
        [Token.colon({0,0}), Token.variable({0,0}, "x")], 
        Logger.new(Logger::Level::Debug)
        )
      parser.consume?(Token::Variable).should be_nil
      parser.consume?(Token::Colon).should eq(Token.colon({0,0}))
      parser.consume?(Token::Variable).should eq(Token.variable({0,0}, "x"))
      parser.eof?.should be_true
      parser.consume?(Token::Variable).should be_nil
    end
    it "consumes the current token if it has the specified data" do
      parser = Parser.new(
        [
         Token.variable({0,0}, "x"),
         Token.operator({0,0}, Operator::Assign),
         Token.keyword({0,0}, KeyWord::If)
        ], 
        Logger.new(Logger::Level::Debug)
        )
      parser.consume?(KeyWord::Struct).should be_nil
      parser.consume?("x").should eq(Token.variable({0,0}, "x"))
      parser.consume?("x").should be_nil
      parser.consume?(Operator::Mod).should be_nil
      parser.consume?(Operator::Assign).should eq(Token.operator({0,0}, Operator::Assign))
      parser.consume?(KeyWord::If).should eq(Token.keyword({0,0}, KeyWord::If))
      parser.eof?.should be_true
    end
    it "works the same way, but ignoring newlines when :skip_newlines is specified" do
      parser = Parser.new(
        [
          Token.newline({0,0}, 0),
          Token.variable({0,0}, "x"),
          Token.newline({0,0}, 0),
          Token.operator({0,0}, Operator::Assign),
          Token.newline({0,0}, 0),
          Token.keyword({0,0}, KeyWord::If),
          Token.newline({0,0}, 0)
        ],
        Logger.new(Logger::Level::Debug)
      )
      parser.consume?(KeyWord::Struct, :skip_newline).should be_nil
      parser.peek.is_a?(Token::Newline).should be_true
      parser.consume?("x").should be_nil
      parser.consume?("x", :skip_newline).should eq(Token.variable({0,0}, "x"))
      parser.peek.is_a?(Token::Newline).should be_true
      parser.consume?("x", :skip_newline).should be_nil
      parser.consume?(Operator::Mod, :skip_newline).should be_nil
      parser.consume?(Operator::Assign, :skip_newline).should eq(Token.operator({0,0}, Operator::Assign))
      parser.consume?(KeyWord::If, :skip_newline).should eq(Token.keyword({0,0}, KeyWord::If))
      parser.consume?(Token::Newline).should eq(Token.newline({0,0}, 0))
      parser.eof?.should be_true
    end
  end

  describe "#parse_type_expression" do
    it "consumes a type token and returns a type node" do
      parser("String").parse_type_expression.should eq(Ast::Type.new(loc, "String"))
    end
    it "parses a type with type arguments, recursively" do
      parser = parser("Option[Int] Result[Option[String], Result[Int, String]]")
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Option", [Ast::Type.new(loc, "Int")]))
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Result", [
        Ast::Type.new(loc, "Option", [Ast::Type.new(loc, "String")]),
        Ast::Type.new(loc, "Result", [
          Ast::Type.new(loc, "Int"),
          Ast::Type.new(loc, "String")
        ])
      ]))
      parser.eof?.should be_true
    end
  end

  describe "#parse_type_parameters?" do
    it "returns nil if no type parameters" do
      parser = parser("")
      type_params = parser.parse_type_parameters?
      type_params.should be_nil
    end
    it "parses type parameters with constraints" do
      parser = parser("[T, U: Constraint, V: Read & Write]")
      type_params = parser.parse_type_parameters?
      type_params.should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "V", [Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")])
      ])
    end
    it "parses type parameters with constraints with optional colon" do
      parser = parser("[T, U Constraint, V Read & Write]")
      type_params = parser.parse_type_parameters?
      type_params.should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "V", [Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")])
      ])
    end
    it "adds type parameters to inherited type parameters" do
      original_type_params = [
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")])
      ]
      parser = parser("[A] [B]")
      type_params = parser.parse_type_parameters?(original_type_params)
      type_params.should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "A")
      ])
      type_params = parser.parse_type_parameters?(original_type_params)
      type_params.should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "B")
      ])
      type_params = parser.parse_type_parameters?(original_type_params)
      type_params.should eq(original_type_params)
    end
    it "returns empty array if empty type parameters" do
      parser = parser("[]")
      type_params = parser.parse_type_parameters?
      type_params.should eq([] of Ast::TypeParameter)
    end
  end

  describe "#parse_type_args?" do
    it "returns nil if no type arguments" do
      parser = parser("")
      type_args = parser.parse_type_args?
      type_args.should be_nil
    end
    it "parses one type argument" do
      parser = parser("[T]")
      type_args = parser.parse_type_args?
      type_args.should eq([Ast::Type.new(loc(1, 2), "T")])
    end
    it "parses multiple type arguments" do
      parser = parser("[T, U, V, String, Int32]")
      type_args = parser.parse_type_args?
      type_args.should eq([
        Ast::Type.new(loc(1, 2), "T"), 
        Ast::Type.new(loc(1, 5), "U"), 
        Ast::Type.new(loc(1, 8), "V"), 
        Ast::Type.new(loc(1, 11), "String"), 
        Ast::Type.new(loc(1, 18), "Int32")])
      parser.eof?.should be_true
    end
  end

  describe "#parse_signature" do
    it "parses type parameters, parameters, and return type" do
      parser = parser("[T] (x : Int32) Int32")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, [Ast::TypeParameter.new(loc, "T")], [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], Ast::Type.new(loc, "Int32")))
    end
    it "type parameters are nil if missing, or empty if empty" do
      parser = parser("(x : Int32) Int32")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, nil, [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], Ast::Type.new(loc, "Int32")))
      parser = parser("[] (x : Int32) Int32")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, [] of Ast::TypeParameter, [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], Ast::Type.new(loc, "Int32")))
    end
    it "parameters are nil if missing, or empty if empty" do
      parser = parser("Int32")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, nil, nil, Ast::Type.new(loc, "Int32")))
      parser = parser("() Int32")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, nil, [] of Ast::Parameter, Ast::Type.new(loc, "Int32")))
    end
    it "return type is nil if missing" do
      parser = parser("(x : Int32)")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(loc, nil, [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], nil))
    end
    it "newlines act as delimiters in type paramters and parameters" do
      parser = parser("[T\nU] (x Int\ny Float)")
      sig = parser.parse_signature(loc)
      sig.should eq(Ast::Signature.new(
        loc, 
        [Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U")], 
        [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int")), Ast::Parameter.new(loc, "y", Ast::Type.new(loc, "Float"))], 
        nil
      ))
    end
    it "is basically white-space insensitive" do
      # parser = parser("[T] (x : Int32) Int32")
      # sig = parser.parse_signature(loc)
      # sig.should eq(Ast::Signature.new(loc, [Ast::TypeParameter.new(loc, "T")], [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], Ast::Type.new(loc, "Int32")))
    end
  end
end

class TokenCollector
  include Lexer::Out
  property tokens = [] of Token
  def emit(token : Token)
    @tokens << token
  end
  def signal_end_of_file
  end
  def send_log(log : Logger)
  end
  def import(import_loc : Location, path : String, import_alias : String?, import_bindings : Array({String, String})?)
    import = Ast::Import.new(import_loc, path, import_alias, import_bindings)
    puts import
  end
end
    

<<-MISMO
struct Option[T]: String & Size

extend[T: String] T: Format
  def format String: 
    self.string

extend Format
MISMO