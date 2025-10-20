require "./spec_helper"
# require "../src/parser"
# require "../src/lexer"


describe Parser do
  describe "#consume?" do
    it "consumes the current token of the specified type, otherwise nil" do
      parser = parser(": x")
      parser.consume?(Token::Variable).should be_nil
      parser.consume?(Token::Colon).should eq(Token.colon({1,1}))
      parser.consume?(Token::Variable).should eq(Token.variable({1,3}, "x"))
      parser.consume?(Token::Variable).should be_nil
      parser.consume?(Token::Newline).should eq(Token.newline({1,4}, 0))
      parser.eof?.should be_true
    end
    it "consumes the current token if it has the specified data" do
      parser = parser("x := if")
      parser.consume?(KeyWord::If).should be_nil
      parser.consume?("x").should eq(Token.variable({1,1}, "x"))
      parser.consume?("x").should be_nil
      parser.consume?(Operator::Mod).should be_nil
      parser.peek.should eq(Token.operator({1,3}, Operator::Assign))
      parser.consume?(Operator::Assign).should eq(Token.operator({1,3}, Operator::Assign))
      parser.consume?(KeyWord::If).should eq(Token.keyword({1,6}, KeyWord::If))
      parser.consume?(Token::Newline).should_not be_nil
      parser.eof?.should be_true
    end
  end

  describe "#parse_constraints" do
    it "returns no constraints if no types given" do
      parser("this aint no constraint").parse_constraints.should eq(Ast::Constraints.new)
    end
    it "parses a type as a positive constraint" do
      parser("String]").parse_constraints.should eq(
        Ast::Constraints.include(Ast::Type.new(loc, "String")))
    end
    it "parses multiple &-separated types as constraints" do
      parser("Compare & Read & Format]").parse_constraints.should eq(
        Ast::Constraints.new(Slice[Ast::Type.new(loc, "Compare"), Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Format")]))
    end
    it "parses bounds prefixed with ~ as prohibitions" do
      parser("Read ~Write]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "Read")], 
          Slice[Ast::Type.new(loc, "Write")]))
    end
    it "parses bounds prefixed with ~ as prohibitions, with multiple prohibitions" do
      parser("Read ~Write ~Format]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "Read")], 
          Slice[Ast::Type.new(loc, "Write"), Ast::Type.new(loc, "Format")]))
    end
    it "the first bound may be negative" do
      parser("~Read]").parse_constraints.should eq(
        Ast::Constraints.exclude(Ast::Type.new(loc, "Read")))
    end
    it "the first bound may be negative, with multiple prohibitions" do
      parser("~Read ~Write]").parse_constraints.should eq(
        Ast::Constraints.new(excludes: Slice[Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")]))
    end
    it "the first bound may be negative, with a positive constraint" do
      parser("~Read & String]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "String")],
          Slice[Ast::Type.new(loc, "Read")]))
    end
    it "required and prohibited constraints can be mixed " do
      parser("~Read & String ~ Int & Format]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "String"), Ast::Type.new(loc, "Format")],
          Slice[Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Int")]))
    end
    it "parses constraints after an optional 'is' or ':'" do
      parser("is Compare]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "Compare")]))
      parser(": Compare]").parse_constraints.should eq(
        Ast::Constraints.new(
          Slice[Ast::Type.new(loc, "Compare")]))
    end
  end

  describe "#parse_type_expression" do
    it "consumes a type token and returns a type node" do
      parser("String").parse_type_expression.should eq(Ast::Type.new(loc, "String"))
    end
    it "parses a type with type arguments, recursively" do
      parser = parser("Option[Int] Result[Option[String], Result[Int, String]]")
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Option", Slice[Ast::Type.new(loc, "Int")]))
      parser.parse_type_expression.should eq(Ast::Type.new(loc, "Result", Slice[
        Ast::Type.new(loc, "Option", Slice[Ast::Type.new(loc, "String")]),
        Ast::Type.new(loc, "Result", Slice[
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
      parser("").parse_type_parameters?.should be_empty
    end
    it "parses type parameters with constraints" do
      parser("[T, U: Constraint, V: Read & Write]").parse_type_parameters?.should eq(Slice[
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", Slice[Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "V", Slice[Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")])
      ])
    end
    it "parses type parameters with constraints with optional colon" do
      parser("[T, U Constraint, V Read & Write]").parse_type_parameters?.should eq(Slice[
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", Slice[Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "V", Slice[Ast::Type.new(loc, "Read"), Ast::Type.new(loc, "Write")])
      ])
    end
    it "adds type parameters to inherited type parameters" do
      original_type_params = Slice[
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", Slice[Ast::Type.new(loc, "Constraint")])
      ]
      parser = parser("[A] [B]")
      parser.parse_type_parameters?(original_type_params).should eq(Slice[
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", Slice[Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "A")
      ])
      parser.parse_type_parameters?(original_type_params).should eq(Slice[
        Ast::TypeParameter.new(loc, "T"), 
        Ast::TypeParameter.new(loc, "U", Slice[Ast::Type.new(loc, "Constraint")]),
        Ast::TypeParameter.new(loc, "B")
      ])
      parser.parse_type_parameters?(original_type_params).should eq(original_type_params)
    end
    it "returns empty array if empty type parameters" do
      parser("[]").parse_type_parameters?.should eq(Slice(Ast::TypeParameter).empty)
    end
  end

  describe "#parse_type_args?" do
    it "returns nil if no type arguments" do
      parser("").parse_type_args?.should eq(Slice(Ast::Type).empty)
    end
    it "parses one type argument" do
      parser("[T]").parse_type_args?.should eq(Slice[Ast::Type.new(loc(1, 2), "T")])
    end
    it "parses multiple type arguments" do
      parser = parser("[T, U, V, String, Int32]")
      parser.parse_type_args?.should eq(Slice[
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
      parser("[T] (x : Int32) Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          Slice[Ast::TypeParameter.new(loc, "T")], 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "type parameters are nil if missing, or empty if empty" do
      parser("(x : Int32) Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
      parser("[] (x : Int32) Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "parameters are nil if missing, or empty if empty" do
      parser("Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [] of Ast::Parameter, 
          Ast::Type.new(loc, "Int32")))
      parser("() Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [] of Ast::Parameter, 
          Ast::Type.new(loc, "Int32")))
    end
    it "return type is nil if missing" do
      parser("(x : Int32)")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          nil, 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          nil))
    end
    it "newlines act as delimiters in type parameters and parameters" do
      parser("newline-type-params", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
          [
            T
            U
          ]
          (
            x Int
            y Float
          )
        MISMO
      )
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U")], 
        [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int")), Ast::Parameter.new(loc, "y", Ast::Type.new(loc, "Float"))], 
        nil
      ))
    end
    it "is basically white-space insensitive" do
      parser("[T] (x : Int32) Int32")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
          loc, 
          Slice[Ast::TypeParameter.new(loc, "T")], 
          [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "Int32"))], 
          Ast::Type.new(loc, "Int32")))
    end
    it "parses conventions before parameter names and before return type" do
      parser("(mut a A, let b B, ref c C, move d D) let Return")
      .parse_signature(loc)
      .should eq(Ast::Signature.new(
        loc, 
        nil, 
        [
          Ast::Parameter.new(loc(1, 2), Mode::Mut, "a", Ast::Type.new(loc(1, 8), "A")),
          Ast::Parameter.new(loc(1, 11), Mode::Let, "b", Ast::Type.new(loc(1, 17), "B")),
          Ast::Parameter.new(loc(1, 20), Mode::Ref, "c", Ast::Type.new(loc(1, 26), "C")),
          Ast::Parameter.new(loc(1, 29), Mode::Move, "d", Ast::Type.new(loc(1, 36), "D"))
        ], 
        Ast::Type.new(loc(1, 40), "Return"),
        Mode::Let
      ))
    end
  end

  describe "#parse_method" do
    self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Object"))
    it "parses a method with a receiver and one other parameter with a return type" do
      parser = parser("def do_something(param Int32) -> Int32: --empty body")
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      parser.next_token.should eq(Token.variable({1, 1}, "def"))
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
      parser = parser("def mut change: --empty body")
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      parser.next_token.should eq(Token.variable({1, 1}, "def"))
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
      parser = parser("def +(other Self) -> Self: --empty body")
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      parser.next_token.should eq(Token.variable({1, 1}, "def"))
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
      parser = parser("def get_age -> Int: --empty body")
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      parser.next_token.should eq(Token.variable({1, 1}, "def"))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "get_age", 
        Ast::Signature.new(
          loc, 
          nil, 
          [self_param], 
          Ast::Type.new(loc, "Int")
        ),
        [] of Ast::Expr
      ))
    end
    it "parses a method that acts as a constructor for another type" do
      parser = parser("def String String: --empty body")
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Def))
      parser.next_token.should eq(Token.variable({1, 1}, "def"))
      method = parser.parse_method(loc, self_param)
      method.should eq(Ast::Function.new(
        loc, 
        "String", 
        Ast::Signature.new(
          loc, 
          nil, 
          [self_param], 
          Ast::Type.new(loc, "String")
        ),
        [] of Ast::Expr
      ))
    end
  end

  describe "#parse_type_header" do
    it "returns the convention, name, type parameters, and traits" do
      parser("Type[T] is Trait1 & Trait2")
      .parse_type_header
      .should eq({
        nil, 
        "Type", 
        Slice[Ast::TypeParameter.new(loc, "T")], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
    it "returns the convention, name, type parameters, and traits with a receiver convention" do
      parser("mut Type[T Bound] is Trait1 & Trait2")
      .parse_type_header
      .should eq({
        Mode::Mut, 
        "Type", 
        Slice[Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "Bound")])], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
    it "parses a type header with some newlines thrown in there" do
      parser("mut Type[\n T\n] is \n Trait1 \n &\n Trait2\n")
      .parse_type_header
      .should eq({
        Mode::Mut, 
        "Type", 
        Slice[Ast::TypeParameter.new(loc, "T")], 
        [Ast::Type.new(loc, "Trait1"), Ast::Type.new(loc, "Trait2")]
      })
    end
  end

  describe "#parse_struct" do
    it "parses a struct with fields" do
      code = <<-MISMO
        struct Point
          var x Int
          var y Int
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.next_token.should eq(Token.variable({1, 1}, "struct"))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Point", 
        fields: [
          Ast::Field.new(loc, "x", Ast::Type.new(loc, "Int")),
          Ast::Field.new(loc, "y", Ast::Type.new(loc, "Int"))
        ]
      ))
    end
    it "parses a struct with fields and type parameters" do
      code = <<-MISMO
        struct Array[T]
          var _buffer Pointer[T]
          var _length UInt
          var _capacity UInt
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.next_token.should eq(Token.variable({1, 1}, "struct"))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Array", 
        Slice[Ast::TypeParameter.new(loc, "T")], 
        fields: [
          Ast::Field.new(loc, "_buffer", Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")])),
          Ast::Field.new(loc, "_length", Ast::Type.new(loc, "UInt")),
          Ast::Field.new(loc, "_capacity", Ast::Type.new(loc, "UInt"))
        ]
      ))
    end
    it "parses a struct with traits" do
      code = <<-MISMO
        struct Slice[T] is Sequence[T] & Indexable[T]
          var _buffer Pointer[T]
          var _length UInt
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.next_token.should eq(Token.variable({1, 1}, "struct"))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Slice", 
        Slice[Ast::TypeParameter.new(loc, "T")], 
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
          var string String
          def read Char:
            nil -- string.at(0)
          def read Int:
            nil -- string.at(0)
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.next_token.should eq(Token.variable({1, 1}, "struct"))
      parser.parse_struct(loc)
      .should eq(Ast::Struct.new(
        loc, 
        "Reader", 
        fields: [
          Ast::Field.new(loc, "string", Ast::Type.new(loc, "String"))
        ]
      ))
      # parser.declarations.size.should eq(3)
      read1 = parser.declarations[0]
      read1.should be_a(Ast::Function)
      read1.name.should eq("read")
      read1.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        nil,
        [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Reader"))],
        Ast::Type.new(loc, "Char")
      ))
      read2 = parser.declarations[1]
      read2.should be_a(Ast::Function)
      read2.name.should eq("read")
      read2.as(Ast::Function).signature.should eq(Ast::Signature.new(
          loc,
          nil,
          [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Reader"))],
          Ast::Type.new(loc, "Int")
        )
      )
      read1.should_not eq(read2)
    end
    it "parses a struct with methods and constructors" do
      code = <<-MISMO
        struct Array[T]
          var _buffer Pointer[T]
          var _count UInt
          var _capacity UInt
          
          constructor(cap UInt):
            nil
            -- cap = cap.next_power_of_2
            -- buf = Pointer[T].alloc(cap)
            -- Array[T](buf, 0, cap)
          
          def cap UInt: ._capacity
          def count UInt: ._count
        MISMO
      self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "T")]))
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Struct))
      parser.next_token.should eq(Token.variable({1, 1}, "struct"))
      s = parser.parse_struct(loc)
      s.should eq(Ast::Struct.new(
        loc, 
        "Array", 
        Slice[Ast::TypeParameter.new(loc, "T")], 
        nil,
        [
          Ast::Field.new(loc, "_buffer", Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")])),
          Ast::Field.new(loc, "_count", Ast::Type.new(loc, "UInt")),
          Ast::Field.new(loc, "_capacity", Ast::Type.new(loc, "UInt"))
        ]
      ))
      # parser.declarations.size.should eq(4)
      constructor = parser.declarations[0]
      constructor.should be_a(Ast::Function)
      constructor.name.should eq("Array")
      constructor.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [Ast::Parameter.new(loc, "cap", Ast::Type.new(loc, "UInt"))],
        Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "T")])
      ))
      cap = parser.declarations[1]
      cap.should be_a(Ast::Function)
      cap.name.should eq("cap")
      cap.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [self_param],
        Ast::Type.new(loc, "UInt")
      ))
      count = parser.declarations[2]
      count.should be_a(Ast::Function)
      count.name.should eq("count")
      count.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [self_param],
        Ast::Type.new(loc, "UInt")
      ))
      # parser.declarations.last.should eq(s)
    end
  end

  describe "#parse_enum" do
    it "parses an enum with no variants" do
      code = <<-MISMO
        enum Token
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        variants: [] of Ast::Variant
      ))
    end
    it "parses an enum with basic variants" do
      code = <<-MISMO
        enum Color
          Red
          Blue
          Green
        MISMO
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Color", 
        variants: [
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
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        variants: [
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
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Result", 
        type_params: Slice[Ast::TypeParameter.new(loc, "R"), Ast::TypeParameter.new(loc, "E")], 
        traits: [Ast::Type.new(loc, "Awesome", [Ast::Type.new(loc, "R")]), Ast::Type.new(loc, "Cool", [Ast::Type.new(loc, "E")])],
        variants: [
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
      parser = parser(code)
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Token", 
        variants: [
          Ast::Variant.new(loc, "EOF"),
          Ast::Variant.new(loc, "Var", [{"0", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "Func", [{"caller", Ast::Type.new(loc, "String")}, {"args", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}, {"return", Ast::Type.new(loc, "String")}]),
          Ast::Variant.new(loc, "App", [{"caller", Ast::Type.new(loc, "String")}, {"args", Ast::Type.new(loc, "Array", [Ast::Type.new(loc, "String")])}])
        ]
      ))
    end
    it "parses an enum with methods and stuff" do
      parser = parser("enum-Color", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        enum Color
          Red
          Green
          Blue
          
          def is_special -> Bool:
            true

          static parse(str String) -> Color:
            nil
            -- if str ==
            --   "red": Color.Red
            --   "green": Color.Green
            --   "blue": Color.Blue
            -- else
            --   raise "Invalid color: ${str}"
        MISMO
      )
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Enum))
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Color", 
        variants: [
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
    it "parses an enum with type parameters and methods" do
      parser = parser("enum-Result", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        enum Result[T, E]
          Ok(T)
          Error(E)

          def ok -> Bool:
            self is Ok

        trait Equatable[T]
          def ==(other : T) -> Bool
        MISMO
      )
      parser.next_token.should eq(Token.variable({1, 1}, "enum"))
      e = parser.parse_enum(loc)
      e.should eq(Ast::Enum.new(
        loc, 
        "Result", 
        type_params: Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "E")], 
        variants: [
          Ast::Variant.new(loc, "Ok", [{"0", Ast::Type.new(loc, "T")}]),
          Ast::Variant.new(loc, "Error", [{"0", Ast::Type.new(loc, "E")}])
        ]
      ))
      parser.declarations.size.should eq(2)
      ok = parser.declarations[0]
      ok.should be_a(Ast::Function)
      ok.name.should eq("ok")
      ok.as(Ast::Function).signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "E")],
        [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Result", [Ast::Type.new(loc, "T"), Ast::Type.new(loc, "E")]))],
        Ast::Type.new(loc, "Bool")
      ))
    end
  end

  describe "#parse_trait" do
    self_param = Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "Self"))
    it "parses a trait" do
      parser = parser("trait-IO", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        trait IO
          def read String
          def write(str String)
        MISMO
      )
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Trait))
      parser.next_token.should eq(Token.variable({1, 1}, "trait"))
      t = parser.parse_trait(loc)
      read_method = Ast::AbstractMethod.new(loc, "read", Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "Self")], 
        [self_param], 
        Ast::Type.new(loc, "String")
      ))
      write_method = Ast::AbstractMethod.new(loc, "write", Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "Self")], 
        [self_param, Ast::Parameter.new(loc, "str", Ast::Type.new(loc, "String"))]
      ))
      t.methods[0].should eq(read_method)
      t.methods[1].should eq(write_method)
      t.should eq(Ast::Trait.new(
        loc, 
        "IO", 
        methods: [read_method, write_method]
      ))
    end
    it "parses a trait with convention and type parameters and super traits" do
      parser = parser("trait-Mutable", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        trait mut Mutable[X] is Dangerous & Fun
          def mutate_me(pls Please)
          
          def let get(index Int) -> let Something:
            default + implementation

          def get(index Int) -> mut Something:
            default + implementation              
        MISMO
      )
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Trait))
      parser.next_token.should eq(Token.variable({1, 1}, "trait"))
      t = parser.parse_trait(loc)
      t.convention.should eq(Mode::Mut)
      t.name.should eq("Mutable")
      t.type_params.should eq(Slice[Ast::TypeParameter.new(loc, "Self"), Ast::TypeParameter.new(loc, "X")])
      t.traits.should eq([Ast::Type.new(loc, "Dangerous"), Ast::Type.new(loc, "Fun")])
      mutate_me = t.methods[0]
      mutate_me.name.should eq("mutate_me")
      mutate_me.signature.should eq(Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "Self"), Ast::TypeParameter.new(loc, "X")], 
        [self_param, Ast::Parameter.new(loc, "pls", Ast::Type.new(loc, "Please"))]
      ))
      let_get = t.methods[1]
      let_get.name.should eq("get")
      let_get_self_param = self_param
      let_get_self_param.convention = Mode::Let
      let_get.signature.should eq(Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "Self"), Ast::TypeParameter.new(loc, "X")], 
        [let_get_self_param, Ast::Parameter.new(loc, "index", Ast::Type.new(loc, "Int"))], 
        Ast::Type.new(loc, "Something"),
        Mode::Let
      ))
      let_get.body.should_not be_nil
      mut_get = t.methods[2]
      mut_get.name.should eq("get")
      mut_get.signature.should eq(Ast::Signature.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "Self"), Ast::TypeParameter.new(loc, "X")], 
        [self_param, Ast::Parameter.new(loc, "index", Ast::Type.new(loc, "Int"))], 
        Ast::Type.new(loc, "Something"),
        Mode::Mut
      ))
      mut_get.body.should_not be_nil
    end
  end

  describe "#parse_extend" do
    it "parses an extend" do
      parser = parser("extend-Format", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        extend[T: String] T is Format
          def format String: 
            self.string
        MISMO
      )
      # parser.next_token.should eq(Token.keyword({1, 1}, KeyWord::Extend))
      parser.next_token.should eq(Token.variable({1, 1}, "extend"))
      e = parser.parse_extend(loc)
      e.should eq(Ast::Extend.new(
        loc, 
        Slice[Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "String")])], 
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
        Slice[Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "String")])], 
        [Ast::Parameter.new(loc, "self", Ast::Type.new(loc, "T"))], 
        Ast::Type.new(loc, "String")
      ))
    end
  end

  describe "#parse_def" do
    it "parses a def block" do
      parser = parser("def-block", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def foo:
            1 + 1 
          
        def bar(a Int, b Int):
          a + b
        
        def baz[T]
          (a String, b T):
            a + b
        
          (x T):
            x
        
        def quz
          [T Stringable](a T) String:
            a.String + " string"
        
          [U Intable]
            (a Int, b U) Int:
              a + b.Int
            (a Float, b U) Int:
              a.Int + b.Int
        
          (a String) String:
            a + "string"
        MISMO
      )
      parser.parse
      # parser.declarations.size.should eq(8)
      parser.declarations.each do |dec| 
        dec.should be_a(Ast::Function)
      end
      f1 = parser.declarations[0].as(Ast::Function)
      f1.name.should eq("foo")
      f1.signature.should eq(Ast::Signature.new(
        loc,
        nil
      ))
      f2 = parser.declarations[1].as(Ast::Function)
      f2.name.should eq("bar")
      f2.signature.should eq(Ast::Signature.new(
        loc,
        nil,
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "Int")), Ast::Parameter.new(loc, "b", Ast::Type.new(loc, "Int"))]
      ))
      f3 = parser.declarations[2].as(Ast::Function)
      f3.name.should eq("baz")
      f3.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "String")), Ast::Parameter.new(loc, "b", Ast::Type.new(loc, "T"))]
      ))
      f4 = parser.declarations[3].as(Ast::Function)
      f4.name.should eq("baz")
      f4.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "T"))]
      ))
      quz1 = parser.declarations[4].as(Ast::Function)
      quz1.name.should eq("quz")
      quz1.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T", [Ast::Type.new(loc, "Stringable")])],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "T"))],
        Ast::Type.new(loc, "String")
      ))
      quz2 = parser.declarations[5].as(Ast::Function)
      quz2.name.should eq("quz")
      quz2.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Intable")])],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "Int")), Ast::Parameter.new(loc, "b", Ast::Type.new(loc, "U"))],
        Ast::Type.new(loc, "Int")
      ))
      quz3 = parser.declarations[6].as(Ast::Function)
      quz3.name.should eq("quz")
      quz3.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "U", [Ast::Type.new(loc, "Intable")])],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "Float")), Ast::Parameter.new(loc, "b", Ast::Type.new(loc, "U"))],
        Ast::Type.new(loc, "Int")
      ))
      quz4 = parser.declarations[7].as(Ast::Function)
      quz4.name.should eq("quz")
      quz4.signature.should eq(Ast::Signature.new(
        loc,
        nil,
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "String"))],
        Ast::Type.new(loc, "String")
      ))
    end
    it "parses multiple unrelated functions in one def block, with optional additional type parameters" do
      parser = parser("def-block-2", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def[T, U] 
          foo(a T, b U) T:
            a + b
          bar[S] -> Nil: nil
          baz[V]
            (a V):
              a
            (x V) V:
              x
        
        def
          bar Nil:
            nil
          baz[T]
            (a T):
              a
        MISMO
      )
      parser.parse
      parser.declarations.size.should eq(6)
      parser.declarations.each do |dec| 
        dec.should be_a(Ast::Function)
      end
      foo = parser.declarations[0].as(Ast::Function)
      foo.name.should eq("foo")
      foo.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U")],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "T")), Ast::Parameter.new(loc, "b", Ast::Type.new(loc, "U"))],
        Ast::Type.new(loc, "T")
      ))
      bar = parser.declarations[1].as(Ast::Function)
      bar.name.should eq("bar")
      bar.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U"), Ast::TypeParameter.new(loc, "S")],
        [] of Ast::Parameter,
        Ast::Type.new(loc, "Nil")
      ))
      baz = parser.declarations[2].as(Ast::Function)
      baz.name.should eq("baz")
      baz.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U"), Ast::TypeParameter.new(loc, "V")],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "V"))]
      ))
      baz2 = parser.declarations[3].as(Ast::Function)
      baz2.name.should eq("baz")
      baz2.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T"), Ast::TypeParameter.new(loc, "U"), Ast::TypeParameter.new(loc, "V")],
        [Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "V"))],
        Ast::Type.new(loc, "V")
      ))
      bar2 = parser.declarations[4].as(Ast::Function)
      bar2.name.should eq("bar")
      bar2.signature.should eq(Ast::Signature.new(
        loc,
        return_type: Ast::Type.new(loc, "Nil")
      ))
      baz3 = parser.declarations[5].as(Ast::Function)
      baz3.name.should eq("baz")
      baz3.signature.should eq(Ast::Signature.new(
        loc,
        Slice[Ast::TypeParameter.new(loc, "T")],
        [Ast::Parameter.new(loc, "a", Ast::Type.new(loc, "T"))]
      ))
    end
  end

  describe "#parse" do
    it "parses a bunch of definitions" do
      parser = parser("bunch-of-defs", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        struct IntPoint
          var x Int
          var y Int

          def +(other IntPoint) -> IntPoint:
            IntPoint(x + other.x, y + other.y)

        enum Option[T]
          Some(T)
          None

        enum Result[T, E]
          Ok(T)
          Error(E)

        enum SomeVariants
          Text(String)
          Integer(Int)
          Location(Float, Float)
          MaybeInt(Option[Int])

        def if_none[T, E](opt Option[T], err E) -> Result[T, E]:
          -- if opt is
          --   Some(t): Result.Ok(t)
          --   None: Result.Error(err)

        trait Stringable
          def string(self Self) -> String

        struct HasStringable[T Stringable]
          var item T

          def string(self Self) -> String:
            "(" + item.string + ")"

        enum TryThis
          Bawal(HasStringable[Float])

        MISMO
      )
      items = parser.parse
      # items.each do |item|
      #   puts item
      # end
      items.size.should eq(10)
    end
  end
end

describe ExpressionParser do
  it "parses a simple binary expression" do
    parser = expression_parser("1 + 2")
    parser.parse.should eq(Ast::Binop.new(
      Ast::Int.new(loc, 1),
      Operator::Add,
      Ast::Int.new(loc, 2)
    ))
  end
  it "parses arithmetic expression with number literals" do
    expression_parser("1 + 2 * 3").parse.should eq(Ast::Binop.new(
      Ast::Int.new(loc, 1),
      Operator::Add,
      Ast::Binop.new(
        Ast::Int.new(loc, 2),
        Operator::Mul,
        Ast::Int.new(loc, 3)
      )
    ))
    expression_parser("1 - 2 * 3 + 4").parse.should eq(Ast::Binop.new(
      Ast::Binop.new(
        Ast::Int.new(loc, 1),
        Operator::Sub,
        Ast::Binop.new(
          Ast::Int.new(loc, 2),
          Operator::Mul,
          Ast::Int.new(loc, 3)
        )
      ),
      Operator::Add,
      Ast::Int.new(loc, 4)
    ))
  end
  it "respects operator precedence" do
    parser = expression_parser("op-precedence", 
      line_offset: __LINE__ + 2, 
      code: <<-MISMO
        not this and not that == -1 + - 2 * 3
        foo = bar = baz
        1 + foo, bar = 1, 2, 3
        1 + foo = 3  -- this should raise an error
      MISMO
    )
    parser.log.expect(
      "Relative precedence not defined between + [left] and = [right].  Disambiguate with parentheses."
    )

    parser.parse.should eq(Ast::Binop.new(
      Ast::NotNode.new(loc, Ast::Identifier.new(loc, "this")),
      Operator::And,
      Ast::Binop.new(
        Ast::NotNode.new(loc, Ast::Identifier.new(loc, "that")),
        Operator::Eq,
        Ast::Binop.new(
          Ast::Int.new(loc, -1),
          Operator::Add,
          Ast::Binop.new(
            Ast::Int.new(loc, -2),
            Operator::Mul,
            Ast::Int.new(loc, 3)
          )
        )
      )
    ))
    parser.next_token.should be_a(Token::Newline)
    parser.expect(Expecting::Term)
    parser.parse.should eq(Ast::Binop.new(
      Ast::Identifier.new(loc, "foo"),
      Operator::Assign,
      Ast::Binop.new(
        Ast::Identifier.new(loc, "bar"),
        Operator::Assign,
        Ast::Identifier.new(loc, "baz")
      )
    ))
    parser.next_token.should be_a(Token::Newline)
    parser.expect(Expecting::Term)
    parser.parse.should eq(Ast::Binop.new(
      Ast::Tuple.new(
        loc,
        [
          Ast::Binop.new(
            Ast::Int.new(loc, 1),
            Operator::Add,
            Ast::Identifier.new(loc, "foo")
          ).as(Ast::Expr),
          Ast::Identifier.new(loc, "bar").as(Ast::Expr)
        ]
      ),
      Operator::Assign,
      Ast::Tuple.new(
        loc,
        [
          Ast::Int.new(loc, 1).as(Ast::Expr),
          Ast::Int.new(loc, 2).as(Ast::Expr),
          Ast::Int.new(loc, 3).as(Ast::Expr)
        ]
      )
    ))
    parser.next_token.should be_a(Token::Newline)
    parser.expect(Expecting::Term)
    parser.parse.should eq(Ast::Binop.new(
      Ast::Binop.new(
        Ast::Int.new(loc, 1),
        Operator::Add,
        Ast::Identifier.new(loc, "foo")
      ),
      Operator::Assign,
      Ast::Int.new(loc, 3)
    ))
    parser.log.check_expectations_empty
  end
  it "parses parentheses" do
    parser = expression_parser("(1 + 2) * 3")
    parser.parse.should eq(Ast::Binop.new(
      Ast::Binop.new(
        Ast::Int.new(loc, 1),
        Operator::Add,
        Ast::Int.new(loc, 2)
      ),
      Operator::Mul,
      Ast::Int.new(loc, 3)
    ))
  end
  it "parses static method calls" do
    expression_parser("Pointer.alloc[T](cap)").parse.should eq(Ast::StaticCall.new(
      loc,
      Ast::Type.new(loc, "Pointer"),
      Ast::Call.new(
        loc,
        "alloc",
        Slice[Ast::Type.new(loc, "T")],
        [Ast::Identifier.new(loc, "cap").as(Ast::Expr)]
      )
    ))
    expression_parser("Pointer[T].alloc(cap)").parse.should eq(Ast::StaticCall.new(
      loc,
      Ast::Type.new(loc, "Pointer", [Ast::Type.new(loc, "T")]),
      Ast::Call.new(
        loc,
        "alloc",
        [Ast::Identifier.new(loc, "cap").as(Ast::Expr)]
      )
    ))
  end
  it "parses generic and non-generic function calls" do
    expression_parser("fn-call", "awesome(1, 2, 3)").parse.should eq(Ast::Call.new(
      loc,
      "awesome",
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    expression_parser("generic-fn", "awesome[Int](1)").parse.should eq(Ast::Call.new(
      loc,
      "awesome",
      Slice[Ast::Type.new(loc, "Int")],
      [Ast::Int.new(loc, 1).as(Ast::Expr)]
    ))
  end
  it "parses generic and non-generic method calls" do
    expression_parser("method-call", "self.awesome(1, 2, 3)").parse.should eq(Ast::Call.new(
      loc,
      "awesome",
      [
        Ast::Identifier.new(loc, "self").as(Ast::Expr),
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    expression_parser("generic-method", "self.awesome[Int](1)").parse.should eq(Ast::Call.new(
      loc,
      "awesome",
      Slice[Ast::Type.new(loc, "Int")],
      [
        Ast::Identifier.new(loc, "self").as(Ast::Expr),
        Ast::Int.new(loc, 1).as(Ast::Expr)
      ]
    ))
  end
  it "parses method calls with no arguments" do
    expression_parser("method-call", "self.awesome").parse.should eq(Ast::Call.new(
      loc,
      "awesome",
      [
        Ast::Identifier.new(loc, "self").as(Ast::Expr),
      ]
    ))
  end
  it "parses basic literals" do
    expression_parser("int", "1").parse.should eq(Ast::Int.new(loc, 1))
    expression_parser("float", "1.0").parse.should eq(Ast::Float.new(loc, 1.0))
    expression_parser("string", "\"hello\"").parse.should eq(Ast::String.new(loc, "hello"))
    expression_parser("true", "true").parse.should eq(Ast::True.new(loc))
    expression_parser("false", "false").parse.should eq(Ast::False.new(loc))
    expression_parser("nil", "nil").parse.should eq(Ast::Nil.new(loc))
  end
  it "parses array literals" do
    expression_parser("array", "[1, 2, 3]").parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [
        1
        2
        3
      ]
      MISMO
    expression_parser("newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [
        1,
        2,
        3,
      ]
      MISMO
    expression_parser("comma-newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [
        1,
        2,
        3,      ]
      MISMO
    expression_parser("comma-newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [
        1,
        2
        3
      ]
      MISMO
    expression_parser("comma-newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [
        1,
        2,
        3      ]
      MISMO
    expression_parser("comma-newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
    code = <<-MISMO
      [1,
        2,
        3
        ]
      MISMO
    expression_parser("comma-newline-array", code).parse.should eq(Ast::Array.new(
      loc,
      [
        Ast::Int.new(loc, 1).as(Ast::Expr),
        Ast::Int.new(loc, 2).as(Ast::Expr),
        Ast::Int.new(loc, 3).as(Ast::Expr),
      ]
    ))
  end
  it "parses implicit tuple literals" do
    expression_parser("1, 2, 3, 4").parse.should eq(Ast::Tuple.new([
      Ast::Int.new(loc, 1).as(Ast::Expr),
      Ast::Int.new(loc, 2).as(Ast::Expr),
      Ast::Int.new(loc, 3).as(Ast::Expr),
      Ast::Int.new(loc, 4).as(Ast::Expr)
    ]))
  end
end
    
describe UcsParser do
  describe "#parse" do
    it "parses basic single line conditional" do
      parser = parser("ucs basic", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def main:
          if x > y: r
        MISMO
      )
      items = parser.parse
      items.size.should eq(1)
      main = items[0]
      main.should be_a(Ast::Function)
      body = main.as(Ast::Function).body
      body.size.should eq(1)
      cond = body[0]
      cond.should be_a(Ast::If)
      cond.should eq(Ast::If.new(loc, [
        Ast::UnaryCondition.new(
          loc,
          Ast::Binop.new(
            Ast::Identifier.new(loc, "x"),
            Operator::Gt,
            Ast::Identifier.new(loc, "y")
          ),
          Ast::Block.new(
            loc,
            [Ast::Identifier.new(loc, "r").as(Ast::Expr)]
          )
        ).as(Ast::Condition)
      ]))
    end
    it "parses basic branched conditionals" do
      parser = parser("ucs branched conditions", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def main:
          if x > y: r
    
          if x >
            y: r1
            z: r2

          if x
            > y: r1
            ==
              z: r2
              w: r3
          
          if 
            x > y: r
            x >
              y: r1
              z: r2
            x
              > y: r1
              ==
                z: r2
                w: r3
        MISMO
      )
      items = parser.parse
      items.size.should eq(1)
      main = items[0]
      main.should be_a(Ast::Function)
      body = main.as(Ast::Function).body
      # body.size.should eq(4)
      body.each { |stmt|
        stmt.should be_a(Ast::If)
        if parser.log.level == Logger::Level::Debug
          puts stmt
        end
      }
    end
    it "parses branched conditionals with 'else's" do
      parser = parser("ucs else conditions", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def main:
          if x > y: 
            r
          else: 
            r2

          if x > y: r else: r2
            
          -- these next two should parse identically
          if x >
            y: r1
            z: r2
            else: 
              r3
          
          if x >
            y: r1
            z: r2
          else: 
            r3

          -- the next three should all parse identically
          if x
            > y: r1
            ==
              z: r2
              w: r3
          else: r4

          if x
            > y: r1
            ==
              z: r2
              w: r3
            else: r4

          if x
            > y: r1
            ==
              z: r2
              w: r3
              else: r4
          -- the last three should all parse identically
        MISMO
      )
      items = parser.parse
      items.size.should eq(1)
      main = items[0]
      main.should be_a(Ast::Function)
      body = main.as(Ast::Function).body
      # body.size.should be_gt(4)
      body.each { |stmt|
        stmt.should be_a(Ast::If)
        if parser.log.level == Logger::Level::Debug
          puts stmt
        end
      }
      body[0].should eq(body[1])
      body[2].should eq(body[3])
      body[4].should eq(body[5])
      body[5].should eq(body[6])
    end
    it "parses branched conditionals with unary conditions/tests" do
      parser = parser("ucs unary conditions", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def main:
          if some_boolean: 
            result
          
          if 
            x > y: r
            test: r2
            x >
              y: r1
              z: r2
            x
              > y: r1
              ==
                z: r2
                w: r3
            .another_test:
              r4
        MISMO
      )
      items = parser.parse
      items.size.should eq(1)
      main = items[0]
      main.should be_a(Ast::Function)
      body = main.as(Ast::Function).body
      # body.size.should be_gt(4)
      body.each { |stmt|
        stmt.should be_a(Ast::If)
        if parser.log.level == Logger::Level::Debug
          puts stmt
        end
      }
      if1 = body[0]
      if1.as(Ast::If).conditionals.should eq([
        Ast::Condition.unary(
          loc, 
          Ast::Identifier.new(loc, "some_boolean"), 
          Ast::Block.new([Ast::Identifier.new(loc, "result").as(Ast::Expr)])
        )
      ])
      if2 = body[1]
      if2.as(Ast::If).conditionals.size.should eq(5)
    end
    it "parses branched conditionals with nested tests via 'and' connector" do
      parser = parser("ucs nested 'and' conditions", 
        line_offset: __LINE__ + 2, 
        source: <<-MISMO
        def main:
          if x > y and x > z: r
    
          if x >
            y: r1
            z: r2
            w and some_boolean: r3
            w and
              y == x: r4
              y > x: r42
              x > y: r43
            w and
              y == x and z == y: r5
            w and
              y == x and z == y and some_boolean: 
          
          if 
            x > y and x > z: r
            x >
              y: r1
              z: r2
            x
              > y: r1
              ==
                z: r2
                w: r3
            true and false: "impossible"
            true and
              some_other_test: "maybe"
              yet_another_test: "could be"
              y ==
                0: "true and zero"
                1: "true and one"
          if x
            >
              y and
                y < x: "yes"
                y > x: "no"
                z and
                  z < x: "maybe"
                  z > x: "could be"
                  else: "inner default"
                else:
                  "middle default"
          else:
            "outer default"
          
        
        MISMO
      )
      items = parser.parse
      items.size.should eq(1)
      main = items[0]
      main.should be_a(Ast::Function)
      body = main.as(Ast::Function).body
      # body.size.should be_gt(4)
      body.each { |stmt|
        stmt.should be_a(Ast::If)
        if parser.log.level == Logger::Level::Debug
          puts stmt
        end
      }
    end
  end
end

<<-MISMO
  def main:
    if x > y: r
    
    if x >
      y: r1
      z: r2

    if x
      > y: r1
      ==
        z: r2
        w: r3
    
    if 
      x > y: r
      x >
        y: r1
        z: r2
      x
        > y: r1
        ==
          z: r2
          w: r3




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
    function_call(
      arg1
      arg2
    )

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
    var array = [1, 2, 3]
    mut array.push(4)
    let array.push(4)

  function add(a Int, b Int) Int:
    a + b

  let x = 55
  let y = -140
  let z = add(x, y)


  function increment(mut a Int):
    a += 1

  var x = 0
  increment(x)
  x  --> 1

  struct copy Point
    var x Int
    var y Int

  function main:
    let p = Point.new(1, 2)
    let q = p
    p.x = 3
    q.x  --> 3

  struct Point is ImplicitCopy

  function main:
    function_call   -- Identifier
    function_call() -- Call
    obj.method      -- MethodCall
    obj.method()    -- MethodCall
    .method         -- MethodCall
    .method()       -- MethodCall
    Point[T](1, 2)    -- Call
    Point[T].new(1,2) -- Call
    Point.new(1,2)    -- MethodCall
    Point(1,2)        -- Call
    (arbitrary expression).method  -- MethodCall

    let x = some_expression
      + y * if z > 0:
        1 
      else: if
        z
          ==
            0: 0
          < 0: -1
          > 0: 1

  struct ref Point[
      T
    ] is 
     Copy 
     & 
     Stringable
    var x T
    var y T
  [
    if bool:
      array = [1, 2,
    
   ] + 3
  struct ManyTypeArgs[
      T: String & Size
      U: Format ~ GenericTrait[
    String,
    Int
      ]
    ]

  
  MISMO


