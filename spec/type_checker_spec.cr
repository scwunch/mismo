require "./spec_helper"
require "../src/type_checker"
require "../src/type_env"
require "../src/types"
require "../src/parser"
require "../src/lexer"

macro not_nil!(prop)
  {{ prop }}.should_not be_nil
  raise "" unless {{prop}}
end

def parser(
    source : String, 
    level : Logger::Level = Logger::Level::Warning
    )
  parser(nil, source, level)
end
def parser(
    file : (String | Nil),
    source : String, 
    level : Logger::Level = Logger::Level::Warning)
  logger = Logger.new(level, file_path: file)
  lexer = Lexer.new(
    Lexer::Reader.new(source), 
    if level == Logger::Level::Debug
      Logger.new(Logger::Level::Info)
    else
      Logger.new(Logger::Level::Warning)
    end
  )
  Parser.new(lexer, logger)
end
def expression_parser(test_name : String, code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(parser(test_name, code, level))
end
def expression_parser(code : String, level : Logger::Level = Logger::Level::Warning)
  ExpressionParser.new(
    parser: parser(code, level),
    # expression_indent: 2,
    # line_indent: 2,
    # stop: StopAt::Normal
  )
end

def loc(line, column)
  Location.new(line, column)
end
def loc 
  Location.zero
end

def type_env(level : Logger::Level = Logger::Level::Warning)
  TypeEnv.new(Logger.new(level))
end
def type_checker(level : Logger::Level = Logger::Level::Warning)
  TypeContext.new(type_env(level))
end
def type_check_program(code : String, level : Logger::Level = Logger::Level::Warning)
  logger = Logger.new
  type_env = TypeEnv.new(logger)
  logger.level = level
  type_env.type_check_program(parser(code, Logger::Level::Warning).parse)
  type_env
end

describe TypeContext do
  describe "#type_check" do
    it "works" do
      type_checker = type_checker()
      ast : Ast::Expr = expression_parser("nil").parse
      hir = type_checker.type_check(ast)
      type_checker.unify(hir.type, Type.nil).should eq(:ok)
      hir.should eq(Hir::Nil.new(loc))
    end
  end
end

describe TypeEnv do
  describe "#type_check_program" do
    it "works" do
      program = <<-MISMO
        struct Point
          field x Int
          field y Int

          def check_me Int:
            var p = Point(1, 2)
            p.x = 3
            p.y = 4
            p.x + p.y
        MISMO
      items = parser(program).parse
      type_env = type_check_program(program)
      point = type_env.user_types["Point"].as(StructBase)
      check_me_func = type_env.functions["check_me"][0]
      check_me_func.return_type.should eq(Type.int)
      param = check_me_func.parameters[0]
      param.mode.should eq(Mode::Let)
      param.name.should eq("self")
      param.type.should eq(Type.struct(point))
      check_me_func.parameters.should eq(
        [Parameter.new(loc, Mode::Let, "self", Type.struct(point))])
    end
  end

  describe "#register_types_and_functions" do
    it "works" do
      program = <<-MISMO
        struct Point
          field x Int
          field y Int
        
        enum Result[T, E]
          Ok(T)
          Error(E)

          def ok -> Bool:
            self is Ok

        trait Equatable[T]
          def ==(other : T) -> Bool

        def main:
          nil
        MISMO
      items = parser(program).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.functions["main"]?.should be_a(Array(FunctionBase))
      type_env.user_types["Point"]?.should be_a(StructBase)
      (result = type_env.user_types["Result"]?).should be_a(EnumBase)
      not_nil! result
      result.type_params.should eq(
        [TypeParameter.new(loc, "T"), TypeParameter.new(loc, "E")])
      type_env.functions["ok"]?.should be_a(Array(FunctionBase))
      type_env.functions["ok"].size.should eq(1)
      ok_func = type_env.functions["ok"][0]
      ok_func.type_params.should eq(
        [TypeParameter.new(loc, "T"), TypeParameter.new(loc, "E")])
      (equatable = type_env.traits["Equatable"]?).should be_a(TraitBase)
      not_nil! equatable
      equatable.type_params.should eq(
        [TypeParameter.new(loc, "T")])
    end
    it "registers trait claims properly" do
      program = <<-MISMO
        trait Stringable
          def String String

        trait Sequence[T] is Iterable[T]
          def get(index UInt) T

        trait Iterable[T]
          def iter Generator[T]

        struct List[T] is Sequence[T]
          def iter Generator[T]:
            Generator(self)

        extend[T] List[T] is Stringable
          def String String:
            "List"

        extend[T] Array[T] is Stringable & Sequence[T]
          def String String: "Array"
          def get(index UInt) T: nil

        extend Array[Int] is Sequence[Int]
        MISMO
      items = parser(program).parse
      type_env = TypeEnv.new(Logger.new())
      type_env.register_types_and_collect_items(items)
      type_env.eval_type_params_and_trait_claims(items)
      stringable = type_env.traits["Stringable"]
      sequence = type_env.traits["Sequence"]
      iterable = type_env.traits["Iterable"]
      list = type_env.user_types["List"].as StructBase
      [
        TraitClaim.new(
          loc,
          [TypeParameter.new(loc, "T")],
          Type.struct(list, [Type.var(0).as Type]),
          [Trait.new(stringable)]
        ),
        TraitClaim.new(
          loc,
          [TypeParameter.new(loc, "T")],
          Type.struct(list, [Type.var(0).as Type]),
          [Trait.new(sequence, [Type.var(0).as Type])]
        ),
        TraitClaim.new(
          loc,
          [TypeParameter.new(loc, "T")],
          Type.array(Type.var(0).as Type),
          [Trait.new(stringable), Trait.new(sequence, [Type.var(0).as Type])]
        ),
        TraitClaim.new(
          loc,
          [] of TypeParameter,
          Type.array(Type.int),
          [Trait.new(sequence, [Type.int.as Type])]
        )
      ].each do |claim|
        type_env.trait_claims.should contain(claim)
      end
      # type_env.trait_claims.each do |claim|
      #   p! claim
      #   print '\n'
      # end
    end
  end

  describe "#implementations" do
    it "hashing works" do
      type_env = TypeEnv.new(Logger.new())
      impls = type_env.implementations
      impls.should eq({} of {Type, Trait} => Implements)
      stringable = TraitBase.new(loc, Mode::Let, "Stringable")
      impls[{Type.int, Trait.new(stringable)}] = Implements::True
      impls[{Type.int, Trait.new(stringable)}].should eq(Implements::True)
      impls[{Type.int, Trait.new(stringable)}].ok?.should be_true

      impls[{Type.array(Type.int), Trait.new(stringable)}] = Implements::True
      impls[{Type.array(Type.int), Trait.new(stringable)}].should eq(Implements::True)
      impls[{Type.array(Type.int), Trait.new(stringable)}].ok?.should be_true

      impls[{Type.array(Type.int), Trait.new(stringable)}] = Implements::False
      impls[{Type.array(Type.int), Trait.new(stringable)}].should eq(Implements::False)
      impls[{Type.array(Type.int), Trait.new(stringable)}].ok?.should be_false

      equatable_base_trait = TraitBase.new(loc, Mode::Let, "Equatable", [TypeParameter.new(loc, "T")])
      equatable = ->(type : Type) do
        Trait.new(equatable_base_trait, [type])
      end
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}] = Implements::True
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}].should eq(Implements::True)
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}].ok?.should be_true
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(1))}]? .should be_nil

      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}] = Implements::False
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}].should eq(Implements::False)
      impls[{Type.array(Type.var(0)), equatable.call(Type.var(0))}].ok?.should be_false
    end
  end
  describe "#check_trait_implementations" do
    it "verifies basic trait implementations requiring zero or one type parameters, and up to two methods" do
      program = <<-MISMO
        trait Trivial

        trait Stringable
          def String String

        extend Int is Trivial & Stringable
          def String String:
            "Int"

        trait Sequence[T] is Iterable[T]
          def get(index UInt) T

        trait Iterable[T]
          def iter Generator[T]


        extend[T] Array[T] is Sequence[T]
          def get(index UInt) T:
            nil

          def iter Generator[T]:
            Generator(self)

            
        extend[T] Array[T] is Stringable
          def String String:
            "Array"        
        MISMO
      items = parser(program).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.check_trait_implementations
      trivial = type_env.traits["Trivial"]
      stringable = type_env.traits["Stringable"]
      sequence = type_env.traits["Sequence"]
      type_env.implementations[{Type.int, Trait.new(trivial)}]?.should eq(Implements::True)
      type_env.implementations[{Type.int, Trait.new(stringable)}]?.should eq(Implements::True)
      type_env.implementations[{Type.array(Type.var(0)), Trait.new(sequence, [Type.var(0).as Type])}]?.should eq(Implements::True)
      type_env.implementations[{Type.array(Type.var(0)), Trait.new(stringable)}]?.should eq(Implements::True)
      # type_env.trait_claims.each do |claim|
      #   print claim; print '\n'
      # end
      # type_env.implementations.each do |key, value|
      #   print "#{key[0]} is #{key[1]} => #{value}\n"
      # end
    end
    it "verifies more complex trait implementations" do
      program = <<-MISMO
        trait Trivial

        trait Marker
          def is_marker Bool: true

        trait Equatable[T Equatable[Self]]
          def ==(other : T) -> Bool

        trait SelfEquatable
          def ==(other Self) -> Bool

        extend Int is Equatable[Int] & SelfEquatable & Marker
          def ==(other : Int) -> Bool:
            self == other   
        MISMO
      items = parser(program).parse
      type_env = TypeEnv.new(Logger.new(:debug))
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.check_trait_implementations
      trivial = type_env.traits["Trivial"]
      marker = type_env.traits["Marker"]
      equatable = type_env.traits["Equatable"]
      self_equatable = type_env.traits["SelfEquatable"]
      ctx = TypeContext.new(type_env)
      type_env.implementations[{Type.int, Trait.new(trivial)}]?.should be_nil
      ctx.type_implements_trait?(Type.int, Trait.new(trivial)).should be_true
      type_env.implementations[{Type.int, Trait.new(trivial)}]?.should eq(Implements::True)
      type_env.implementations[{Type.int, Trait.new(marker)}]?.should eq(Implements::True)
      ctx.type_implements_trait?(Type.int, Trait.new(marker)).should be_true
      ctx.type_implements_trait?(Type.int, Trait.new(equatable, [Type.int.as Type])).should be_true
      type_env.implementations.each do |impl|
        print impl; print '\n'
        trait = impl[0][1]
        if trait.type_args.size > trait.base.type_params.size
          p! trait.to_s == "trait Equatable[Int, T1]"
          raise "break: #{trait}"
        end
      end
      type_env.implementations[{Type.int, Trait.new(equatable, [Type.int.as Type])}]?.should eq(Implements::True)
      type_env.implementations[{Type.int, Trait.new(self_equatable)}]?.should eq(Implements::True)
    end
  end
end