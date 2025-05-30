require "./spec_helper"
require "../src/parser"
require "../src/lexer"

def parser(
    source : String, 
    context : ParserContext,
    level : Logger::Level = Logger::Level::Warning
  )
  # level = Logger::Level::Debug
  logger = Logger.new(level)
  lexer = Lexer.new(
    Lexer::Reader.new(source), 
    if level == Logger::Level::Debug
      Logger.new(Logger::Level::Info)
    else
      logger
    end
  )
  Parser.new(lexer, logger).tap { |p| p.context = context }
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
      parser = parser(": x", :block)
      parser.consume?(Token::Variable).should be_nil
      parser.consume?(Token::Colon).should eq(Token.colon({1,1}))
      parser.consume?(Token::Variable).should eq(Token.variable({1,3}, "x"))
      parser.consume?(Token::Variable).should be_nil
      parser.consume?(Token::Newline).should eq(Token.newline({1,4}, 0))
      parser.eof?.should be_true
    end
    it "consumes the current token if it has the specified data" do
      parser = parser("x := if", :block)
      parser.consume?(KeyWord::Struct).should be_nil
      parser.consume?("x").should eq(Token.variable({1,1}, "x"))
      parser.consume?("x").should be_nil
      parser.consume?(Operator::Mod).should be_nil
      parser.peek.should eq(Token.operator({1,3}, Operator::Assign))
      parser.consume?(Operator::Assign).should eq(Token.operator({1,3}, Operator::Assign))
      parser.consume?(KeyWord::If).should eq(Token.keyword({1,6}, KeyWord::If))
      parser.consume?(Token::Newline).should_not be_nil
      parser.eof?.should be_true
    end
    it "works the same way, but ignoring newlines when context is ParserContext::TopLevel" do
      parser = parser("\nx\n:=\ndef", :top_level)
      parser.consume?(KeyWord::Struct).should be_nil
      parser.consume?(Token::Newline).should be_nil  # newlines are not emitted at top level
      parser.consume?("x").should eq(Token.variable({2,1}, "x"))
      parser.consume?("x").should be_nil
      parser.consume?(Operator::Assign).should eq(Token.operator({3, 1}, Operator::Assign))
      parser.consume?(KeyWord::Def).should eq(Token.keyword({4, 1}, KeyWord::Def))
      parser.consume?(Token::Newline).should be_nil
      parser.eof?.should be_true
    end
  end

  describe "#parse_constraints" do
    it "returns no constraints if no types given" do
      parser("this aint no constraint", :block).parse_constraints.should eq(Ast::Constraints.new)
    end
    it "parses a type as a positive constraint" do
      parser("String", :block).parse_constraints.should eq(
        Ast::Constraints.new([Ast::Type.new(loc, "String")]))
    end
    it "parses multiple &-separated types as constraints" do
      parser("Compare & Read & Format", :block).parse_constraints.should eq(
        Ast::Constraints.new([Ast::Type.new(loc, "Compare"), Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Format")]))
    end
    it "parses bounds prefixed with ~ as prohibitions" do
      parser("Read ~Write", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "Read")], 
          [Ast::Type.new(loc, "Write")]))
    end
    it "parses bounds prefixed with ~ as prohibitions, with multiple prohibitions" do
      parser("Read ~Write ~Format", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "Read")], 
          [Ast::Type.new(loc, "Write"), Ast::Type.new(loc, "Format")]))
    end
    it "the first bound may be negative" do
      parser("~Read", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          nil, 
          [Ast::Type.new(loc, "Read")]))
    end
    it "the first bound may be negative, with multiple prohibitions" do
      parser("~Read ~Write", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          nil,
          [Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")]))
    end
    it "the first bound may be negative, with a positive constraint" do
      parser("~Read & String", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "String")],
          [Ast::Type.new(loc, "Read")]))
    end
    it "required and prohibited constraints can be mixed " do
      parser("~Read & String ~ Int & Format", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "String"), Ast::Type.new(loc, "Format")],
          [Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Int")]))
    end
    it "parses constraints after an optional 'is' or ':'" do
      parser("is Compare", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "Compare")]))
      parser(": Compare", :block).parse_constraints.should eq(
        Ast::Constraints.new(
          [Ast::Type.new(loc, "Compare")]))
    end
  end

  describe "#parse_type_expression" do
    it "consumes a type token and returns a type node" do
      parser("String", :block).parse_type_expression.should eq(Ast::Type.new(loc, "String"))
    end
    it "parses a type with type arguments, recursively" do
      parser = parser("Option[Int] Result[Option[String], Result[Int, String]]", :block)
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Option", [Ast::Type.new(loc, "Int")]))
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Result", [
        Ast::Type.new(loc, "Option", [Ast::Type.new(loc, "String")]),
        Ast::Type.new(loc, "Result", [
          Ast::Type.new(loc, "Int"),
          Ast::Type.new(loc, "String")
        ])
      ]))
      parser.consume?(Token::Newline).should_not be_nil
      parser.eof?.should be_true
    end
  end

  describe "#parse_type_parameters?" do
    it "returns nil if no type parameters" do
      parser("", :block).parse_type_parameters?.should be_nil
    end
    it "parses type parameters with constraints" do
      parser("[T, U: Constraint, V: Read & Write]", :block).parse_type_parameters?.should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "V", [Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")])
      ])
    end
    it "parses type parameters with constraints with optional colon" do
      parser("[T, U Constraint, V Read & Write]", :block).parse_type_parameters?.should eq([
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
      parser = parser("[A] [B]", :block)
      parser.parse_type_parameters?(original_type_params).should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "A")
      ])
      parser.parse_type_parameters?(original_type_params).should eq([
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "B")
      ])
      parser.parse_type_parameters?(original_type_params).should eq(original_type_params)
    end
    it "returns empty array if empty type parameters" do
      parser("[]", :block).parse_type_parameters?.should eq([] of Ast::TypeParameter)
    end
  end

  describe "#parse_type_args?" do
    it "returns nil if no type arguments" do
      parser("", :block).parse_type_args?.should be_nil
    end
    it "parses one type argument" do
      parser("[T]", :block).parse_type_args?.should eq([Ast::Type.new(loc(1, 2), "T")])
    end
    it "parses multiple type arguments" do
      parser = parser("[T, U, V, String, Int32]", :block)
      parser.parse_type_args?.should eq([
        Ast::Type.new(loc(1, 2), "T"), 
        Ast::Type.new(loc(1, 5), "U"), 
        Ast::Type.new(loc(1, 8), "V"), 
        Ast::Type.new(loc(1, 11), "String"), 
        Ast::Type.new(loc(1, 18), "Int32")])
      parser.consume?(Token::Newline).should_not be_nil
      parser.eof?.should be_true
    end
  end

  describe "#parse_signature" do
    it "parses type parameters, parameters, and return type" do
      parser("[T] (x : Int32) Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          [Ast::TypeParameter.new(loc, "T")], 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "type parameters are nil if missing, or empty if empty" do
      parser("(x : Int32) Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
      parser("[] (x : Int32) Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          [] of Ast::TypeParameter, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "parameters are nil if missing, or empty if empty" do
      parser("Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          nil, 
          Ast::Type.new(loc, "Int32")))
      parser("() Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [] of Ast::Parameter, 
          Ast::Type.new(loc, "Int32")))
    end
    it "return type is nil if missing" do
      parser("(x : Int32)", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          nil))
    end
    it "newlines act as delimiters in type parameters and parameters" do
      code = <<-MISMO
          [
            T
            U
          ]
          (
            x Int
            y Float
          )
        MISMO
      parser(code, :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
        loc, 
        [Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U")], 
        [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int")), Ast::Parameter.new(loc, "y", Ast::Type.new(loc, "Float"))], 
        nil
      ))
    end
    it "is basically white-space insensitive" do
      parser("[T] (x : Int32) Int32", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          [Ast::TypeParameter.new(loc, "T")], 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "parses conventions before parameter names and before return type" do
      parser("(mut a A, let b B, ref c C, sink d D) let Return", :top_level)
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
        loc, 
        nil, 
        [
          Ast::Parameter.new(loc(1, 2), Mode::Mut, "a", Ast::Type.new(loc(1, 8), "A")),
          Ast::Parameter.new(loc(1, 11), Mode::Let, "b", Ast::Type.new(loc(1, 17), "B")),
          Ast::Parameter.new(loc(1, 20), Mode::Ref, "c", Ast::Type.new(loc(1, 26), "C")),
          Ast::Parameter.new(loc(1, 29), Mode::Sink, "d", Ast::Type.new(loc(1, 36), "D"))
        ], 
        Ast::Type.new(loc(1, 40), "Return"),
        Mode::Let
      ))
    end
  end

  describe "#parse_method" do
    self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Object"))
    it "parses a method with a receiver and one other parameter with a return type" do
      parser = parser("def do_something(param Int32) -> Int32: --empty body", :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "do_something", 
        Ast::Signature.new(
          loc, 
          nil, 
          [
            self_param,
            Ast::Parameter.new(loc, "param", Ast::Type.new(loc, "Int32"))
          ], 
          Ast::Type.new(loc, "Int32")
        ),
        [] of Ast::Expr
      ))
    end
    it "parses a receiver convention that overrides the default" do
      parser = parser("def mut change: --empty body", :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "change", 
        Ast::Signature.new(
          loc(1,15), 
          nil, 
          [
            Ast::Parameter.new(loc, Mode::Mut, "self", Ast::Type.new(loc, "Object"))
          ]
        ),
        [] of Ast::Expr
      ))
    end
    it "parses a method that overloads an operator" do
      parser = parser("def +(other Self) -> Self: --empty body", :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "+", 
        Ast::Signature.new(
          loc, 
          nil, 
          [
            self_param,
            Ast::Parameter.new(loc, "other", Ast::Type.new(loc, "Self"))
          ], 
          Ast::Type.new(loc, "Self")
        ),
        [] of Ast::Expr
      ))
    end
    it "parses a method that acts as a getter" do
      parser = parser("def get_age -> Int: --empty body", :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "get_age", 
        Ast::Signature.new(
          loc, 
          nil, 
          [
            self_param
          ], 
          Ast::Type.new(loc, "Int")
        ),
        [] of Ast::Expr
      ))
    end
    it "parses a method that acts as a constructor for another type" do
      parser = parser("def String String: --empty body", :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "String", 
        Ast::Signature.new(
          loc, 
          nil, 
          [
            self_param
          ], 
          Ast::Type.new(loc, "String")
        ),
        [] of Ast::Expr
      ))
    end
  end

  describe "#parse_type_header" do
    it "returns the convention, name, type parameters, and traits" do
      parser("Type[T] is Trait1 & Trait2", :top_level)
      .parse_type_header
      .should eq({
        nil, 
        "Type", 
        [Ast::TypeParameter.new(loc, "T")], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
    it "returns the convention, name, type parameters, and traits with a receiver convention" do
      parser("mut Type[T Bound] is Trait1 & Trait2", :top_level)
      .parse_type_header
      .should eq({
        Mode::Mut, 
        "Type", 
        [Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "Bound")])], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
    it "parses a type header with some newlines thrown in there" do
      parser("mut\nType[\nT\n]\n is \nTrait1 \n&\n Trait2\n", :top_level)
      .parse_type_header
      .should eq({
        Mode::Mut, 
        "Type", 
        [Ast::TypeParameter.new(loc, "T")], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
  end

  describe "#parse_struct" do
    it "parses a struct with fields" do
      code = <<-MISMO
        struct Point
          field x Int
          field y Int
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Point", 
        nil, 
        nil,
        [
          Ast::Field.new(loc, "x", Ast::Type.new(loc, "Int")),
          Ast::Field.new(loc, "y", Ast::Type.new(loc, "Int"))
        ]
      ))
    end
    it "parses a struct with fields and type parameters" do
      code = <<-MISMO
        struct Array[T]
          field _buffer Pointer[T]
          field _length UInt
          field _capacity UInt
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Array", 
        [Ast::TypeParameter.new(loc, "T")], 
        nil,
        [
          Ast::Field.new(loc, "_buffer", Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")])),
          Ast::Field.new(loc, "_length", Ast::Type.new(loc, "UInt")),
          Ast::Field.new(loc, "_capacity", Ast::Type.new(loc, "UInt"))
        ]
      ))
    end
    it "parses a struct with traits" do
      code = <<-MISMO
        struct Slice[T] is Sequence[T] & Indexable[T]
          field _buffer Pointer[T]
          field _length UInt
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Slice", 
        [Ast::TypeParameter.new(loc, "T")], 
        [Ast::Type.new(loc, "Sequence", [Ast::Type.new(loc, "T")]), Ast::Type.new(loc, "Indexable", [Ast::Type.new(loc, "T")])],
        [
          Ast::Field.new(loc, "_buffer", Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")])),
          Ast::Field.new(loc, "_length", Ast::Type.new(loc, "UInt"))
        ]
      ))
    end
    it "parses a struct with methods" do
      code = <<-MISMO
        struct Reader
          field string String
          def read Char:
            string[0]
          def read Int:
            string[0]
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.parse_struct(loc)
      .should eq(Ast::Struct.new(
        loc, 
        "Reader", 
        nil, 
        nil,
        [
          Ast::Field.new(loc, "string", Ast::Type.new(loc, "String"))
        ]
      ))
      parser.declarations.size.should eq(3)
      read1 = parser.declarations[0]
      read1.should be_a(Ast::Function)
      read1.name.should eq("read")
      read1.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        nil,
          [
            Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Reader"))
          ],
          Ast::Type.new(loc, "Char")
        )
      )
      read2 = parser.declarations[1]
      read2.should be_a(Ast::Function)
      read2.name.should eq("read")
      read2.as(Ast::Function).signature.should eq(Ast::Signature.new(
          loc,
          nil,
          [
            Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Reader"))
          ],
          Ast::Type.new(loc, "Int")
        )
      )
      read1.should_not eq(read2)
    end
    it "parses a struct with methods and constructors" do
      code = <<-MISMO
        struct Array[T]
          field _buffer Pointer[T]
          field _count UInt
          field _capacity UInt
          
          constructor(cap UInt):
            cap = cap.next_power_of_2
            buf = Pointer[T].alloc(cap)
            Array[T](buf, 0, cap)
          
          def cap UInt: ._capacity
          def count UInt: ._count
        MISMO
      self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "T")]))
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Array", 
        [Ast::TypeParameter.new(loc, "T")], 
        nil,
        [
          Ast::Field.new(loc, "_buffer", Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")])),
          Ast::Field.new(loc, "_count", Ast::Type.new(loc, "UInt")),
          Ast::Field.new(loc, "_capacity", Ast::Type.new(loc, "UInt"))
        ]
      ))
      parser.declarations.size.should eq(4)
      constructor = parser.declarations[0]
      constructor.should be_a(Ast::Function)
      constructor.name.should eq("Array")
      constructor.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        [Ast::TypeParameter.new(loc, "T")],
        [Ast::Parameter.new(loc, "cap", Ast::Type.new(loc, "UInt"))],
        Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "T")])
      ))
      cap = parser.declarations[1]
      cap.should be_a(Ast::Function)
      cap.name.should eq("cap")
      cap.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        [Ast::TypeParameter.new(loc, "T")],
        [self_param],
        Ast::Type.new(loc, "UInt")
      ))
      count = parser.declarations[2]
      count.should be_a(Ast::Function)
      count.name.should eq("count")
      count.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        [Ast::TypeParameter.new(loc, "T")],
        [self_param],
        Ast::Type.new(loc, "UInt")
      ))
      parser.declarations.last.should eq(s)
    end
  end

  describe "#parse_enum" do
    it "parses an enum with no variants" do
      code = <<-MISMO
        enum Token
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        nil, 
        nil,
        [] of Ast::Variant
      ))
    end
    it "parses an enum with basic variants" do
      code = <<-MISMO
        enum Color
          Red
          Blue
          Green
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Color", 
        nil, 
        nil,
        [
          Ast::Variant.new(loc, "Red"),
          Ast::Variant.new(loc, "Blue"),
          Ast::Variant.new(loc, "Green"),
        ]
      ))
    end
    it "parses an enum with variants and fields" do
      code = <<-MISMO
        enum Token
          EOF
          Var(String)
          Func(String, Array[String], String)
          App(String, Array[String])
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        nil, 
        nil,
        [
          Ast::Variant.new(loc, "EOF"),
          Ast::Variant.new(loc, "Var", [{"0", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "Func", [{"0", Ast::Type.new(loc, "String")}, {"1", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}, {"2", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "App", [{"0", Ast::Type.new(loc, "String")}, {"1", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}])
        ]
      ))
    end
    it "parses an enum with type parameters and traits" do
      code = <<-MISMO
        enum Result[R, E] is 
              Awesome[R] & Cool[E]
          Ok(R)
          Err(E)
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Result", 
        [Ast::TypeParameter.new(loc, "R"), Ast::TypeParameter.new(loc, "E")], 
        [Ast::Type.new(loc, "Awesome", [Ast::Type.new(loc, "R")]), Ast::Type.new(loc, "Cool", [Ast::Type.new(loc, "E")])],
        [
          Ast::Variant.new(loc, "Ok", [{"0", Ast::Type.new(loc, "R")}]),
          Ast::Variant.new(loc, "Err", [{"0", Ast::Type.new(loc, "E")}])
        ]
      ))
    end
    it "parses an enum with variants that contain named fields" do
      code = <<-MISMO
        enum Token
          EOF
          Var(String)
          Func(caller: String, args: Array[String], return: String)
          App(caller: String, args: Array[String])
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        nil, 
        nil,
        [
          Ast::Variant.new(loc, "EOF"),
          Ast::Variant.new(loc, "Var", [{"0", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "Func", [{"caller", Ast::Type.new(loc, "String")}, {"args", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}, {"return", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "App", [{"caller", Ast::Type.new(loc, "String")}, {"args", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}])
        ]
      ))
    end
    it "parses an enum with methods and stuff" do
      code = <<-MISMO
        enum Color
          Red
          Green
          Blue
          
          def is_special -> Bool:
            true

          static parse(str String) -> Color:
            if str ==
              "red": Color.Red
              "green": Color.Green
              "blue": Color.Blue
            else
              raise "Invalid color: ${str}"
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Color", 
        nil, 
        nil,
        [
          Ast::Variant.new(loc, "Red"),
          Ast::Variant.new(loc, "Green"),
          Ast::Variant.new(loc, "Blue")
        ]
      ))
      parser.declarations.size.should eq(3)
      special = parser.declarations[0]
      special.should be_a(Ast::Function)
      special.name.should eq("is_special")
      special.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        nil,
        [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Color"))],
        Ast::Type.new(loc, "Bool")
      ))
      parse = parser.declarations[1]
      parse.should be_a(Ast::Function)
      parse.name.should eq("Color.parse")
      parse.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        nil,
        [Ast::Parameter.new(loc, "str", Ast::Type.new(loc, "String"))],
        Ast::Type.new(loc, "Color")
      ))
    end
  end

  describe "#parse_trait" do
    self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Self"))
    it "parses a trait" do
      code = <<-MISMO
        trait IO
          def read String
          def write(str String)
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Trait))
      t = parser.parse_trait(loc)
      t.should eq(Ast::Trait.new(
        loc, 
        "IO", 
        nil, 
        nil,
        [
          Ast::AbstractMethod.new(loc, "read", Ast::Signature.new(loc, nil, [self_param], Ast::Type.new(loc, "String"))),
          Ast::AbstractMethod.new(loc, "write", Ast::Signature.new(loc, nil, [self_param, Ast::Parameter.new(loc, "str", Ast::Type.new(loc, "String"))]))
        ]
      ))
    end
    it "parses a trait with convention and type parameters and super traits" do
      code = <<-MISMO
        trait mut Mutable[X] is Dangerous & Fun
          def mutate_me(pls Please)
          
          def let get(index Int) -> Something:
            default + implementation
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Trait))
      t = parser.parse_trait(loc)
      t.convention.should eq(Mode::Mut)
      t.name.should eq("Mutable")
      t.type_parameters.should eq([Ast::TypeParameter.new(loc, "X")])
      t.traits.should eq([Ast::Type.new(loc, "Dangerous"), Ast::Type.new(loc, "Fun")])
      mutate_me = t.methods[0]
      mutate_me.name.should eq("mutate_me")
      mutate_me.signature.should eq(Ast::Signature.new(
        loc, 
        [Ast::TypeParameter.new(loc, "X")], 
        [self_param, Ast::Parameter.new(loc, "pls", Ast::Type.new(loc, "Please"))]
      ))
      get = t.methods[1]
      get.name.should eq("get")
      get_self_param = self_param
      get_self_param.convention = Mode::Let
      get.signature.should eq(Ast::Signature.new(
        loc, 
        [Ast::TypeParameter.new(loc, "X")], 
        [get_self_param, Ast::Parameter.new(loc, "index", Ast::Type.new(loc, "Int"))], 
        Ast::Type.new(loc, "Something")
      ))
      get.body.should_not be_nil
    end
  end

  describe "#parse_extend" do
    it "parses an extend" do
      code = <<-MISMO
        extend[T: String] T is Format
          def format String: 
            self.string
        MISMO
      parser = parser(code, :top_level)
      parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Extend))
      e = parser.parse_extend(loc)
      e.should eq(Ast::Extend.new(
        loc, 
        [Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "String")])], 
        Ast::Type.new(loc, "T"), 
        [Ast::Type.new(loc, "Format")]
      ))
      parser.declarations.size.should eq(2)
      parser.declarations[0].should be_a(Ast::Extend)
      format = parser.declarations[1]
      format.should be_a(Ast::Function)
      format.name.should eq("format")
      format.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc, 
        [Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "String")])], 
        [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "T"))], 
        Ast::Type.new(loc, "String")
      ))
    end
  end
end
    

<<-MISMO
struct Option[T]: String & Size

extend[T: String] T: Format
  def format String: 
    self.string

extend Format

function do_something(param Int32) -> Int32:
  param + 1

enum Token
  EOF
  Var(String)
  Func(String, Array(String), String)
  App(String, Array(String))
  Number[N](num N)

function main:
  let tok : Token = get_token
  tok = Token.Number(53.2)
  if tok is
    Number[Int](n): 
      print("${n} is an integer")
    Number[Float](f): 
      print("${f} is a float")
    Var(name): 
      print("${name} is a variable")
    Func(name): 
      print("${name} is a function")
    App(func, args): 
      print("${func} is a function, called with ${args.join(", ")}")
    EOF: 
      print("EOF")

enum Dynamic
  Value[T](T)

function main:
  let dyn = Dynamic.Value(5)
  if dyn is Value[Int](n):
    print("dyn is an int")
  if dyn is Value[Float](f):
    print("dyn is a float")

struct Dynamic
  field[T] t T

function main:
  let dyn = Dynamic.new(5)
  if dyn.t is Int(n):
    print("dyn is an int")
  if dyn.t is Float(f):
    print("dyn is a float")
MISMO
