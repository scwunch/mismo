require "./spec_helper"
require "../src/type_checker/*"
# require "../src/type_env"
# require "../src/types"
require "../src/ast/*"
# require "../src/lexer"


describe TypeContext do
  describe "#type_check" do
    it "works" do
      type_checker = type_checker("TypeContext#type_check — works", "nil")
      ast : Ast::Expr = ExpressionParser.new(
        parser: parser("nil", Logger::Level::Warning),
        # expression_indent: 2,
        # line_indent: 2,
        # stop: StopAt::Normal
      ).parse
      hir = type_checker.infer(ast)
      type_checker.check_type(hir.type, Type.nil, loc, ast.location).should be_true
      hir.should eq(Hir::Nil.new(loc))
      type_checker.log.errors.should be_empty
    end
  end

  describe "#type_check_program" do
    it "works" do
      type_env = type_check_program(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        program: <<-MISMO
        struct Point
          field x Int
          field y Int

          def check_me Int:
            var p = Point(1, 2)
            p.x = 3
            p.y = 4
            p.x + p.y

        enum Color
          Red
          Green
          Blue

        def main:
          let r = Color.Red
          if r is 
            Red:
              print("Red!")
            Green:
              print("gren")
            _: 
              print("some other color")
            
        MISMO
      )
      point = type_env.type_defs["Point"].as(StructDef)
      check_me_func = type_env.functions["check_me"][0]
      check_me_func.return_type.should eq(Type.int)
      param = check_me_func.parameters[0]
      param.mode.should eq(Mode::Let)
      param.name.should eq("self")
      param.type.should eq(Type.adt(point))
      check_me_func.parameters.should eq(
        [Parameter.new(loc, Mode::Let, "self", Type.adt(point))])
    end
  end
end

describe TypeEnv do
  describe "#type_check_program" do
    it "works" do
      type_env = type_check_program(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        program: <<-MISMO
        struct Point
          field x Int
          field y Int

          def check_me Int:
            var p = Point(1, 2)
            p.x = 3
            p.y = 4
            p.x + p.y
        MISMO
      )
      point = type_env.type_defs["Point"].as(StructDef)
      check_me_func = type_env.functions["check_me"][0]
      check_me_func.return_type.should eq(Type.int)
      param = check_me_func.parameters[0]
      param.mode.should eq(Mode::Let)
      param.name.should eq("self")
      param.type.should eq(Type.adt(point))
      check_me_func.parameters.should eq(
        [Parameter.new(loc, Mode::Let, "self", Type.adt(point))])
    end

    it "workssss" do
      type_env = type_check_program(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        program: <<-MISMO
        def main:
          let t = true
          let one = 1
          let pi = 3.14
          one + one
          one + pi
          one + t
          pi + t

        enum Option[T]
          Some(T)
          None

          def map[T2](fn Callable[T, T2]) -> Option[T2]:
            if self is
              Some(t): Some(fn(t))
              None: None

        enum Result[T, E]
          Ok(T)
          Error(E)

          def map[T2](fn Callable[T, T2]) -> Result[T2, E]:
            if self is
              Ok(t): Ok(fn(t))
              Error(e): Error(e)

        trait Callable[In, Out]

        
        
        struct Point
          field x Int
          field y Int

          def check_me Int:
            var p = Point(1, 2)
            p.x = 3
            p.y = 4
            p.x + p.y        
        MISMO
      )
    end
  end

  describe "#register_types_and_functions" do
    it "works" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: <<-MISMO
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
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.functions["main"]?.should be_a(Array(FunctionDef))
      type_env.type_defs["Point"]?.should be_a(StructDef)
      not_nil!(result = type_env.type_defs["Result"]?)
      result.should be_a(EnumDef)
      result.type_params.should eq(
        Slice[TypeParameter.new(loc, "T"), TypeParameter.new(loc, "E")])
      type_env.functions["ok"]?.should be_a(Array(FunctionDef))
      type_env.functions["ok"].size.should eq(1)
      ok_func = type_env.functions["ok"][0]
      ok_func.type_params.should eq(
        Slice[TypeParameter.new(loc, "T"), TypeParameter.new(loc, "E")])
      not_nil!(equatable = type_env.traits["Equatable"]?)
      equatable.should be_a(TraitDef)
      equatable.type_params.should eq(
        Slice[TypeParameter.new(loc, "Self"), TypeParameter.new(loc, "T")])
    end
    it "registers trait claims properly" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: program = <<-MISMO
        trait Stringable
          def String String

        trait Sequence[T] is Iterable[T]
          def get(index Nat) T

        trait Iterable[T]
          def iter Generator[T]

        struct List[T] is Sequence[T]
          def iter Generator[T]:
            Generator(self)

        struct Generator[T]
          var state T

        extend[T] List[T] is Stringable
          def String String:
            "List"

        extend[T] Array[T] is Stringable & Sequence[T]
          def String String: "Array"
          def get(index Nat) T: nil

        extend Array[Int] is Sequence[Int]

        struct Array[T]
        MISMO
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.eval_type_params_and_trait_claims(items)
      stringable = type_env.traits["Stringable"]
      sequence = type_env.traits["Sequence"]
      iterable = type_env.traits["Iterable"]
      list = type_env.type_defs["List"].as StructDef
      array = type_env.type_defs["Array"].as StructDef
      [
        TraitClaim.new(
          loc,
          Slice[TypeParameter.new(loc, "T")],
          Trait.new(stringable, Slice[Type.adt(list, Slice[Type.var(0)])])
        ),
        TraitClaim.new(
          loc,
          Slice[TypeParameter.new(loc, "T")],
          Trait.new(sequence, Slice[
            Type.adt(list, Slice[Type.var(0)]), 
            Type.var(0)
          ])
        ),
        TraitClaim.new(
          loc,
          Slice[TypeParameter.new(loc, "T")],
          # Trait.new(stringable, Slice[Type.array(Type.var(0))])
          Trait.new(stringable, Slice[Type.adt(array, Slice[Type.var(0)])])
        ),
        TraitClaim.new(
          loc,
          Slice[TypeParameter.new(loc, "T")],
          # Trait.new(sequence, Slice[Type.array(Type.var(0)), Type.var(0)])
          Trait.new(sequence, Slice[
            Type.adt(array, Slice[Type.var(0)]),
            Type.var(0)
          ])
        ),
        TraitClaim.new(
          loc,
          Slice(TypeParameter).empty,
          # Trait.new(sequence, Slice[Type.array(Type.int), Type.int])
          Trait.new(sequence, Slice[
            Type.adt(array, Slice[Type.int]),
            Type.int
          ])
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
      stringable = TraitDef.new(loc, Mode::Let, "Stringable")
      impls[Trait.new(stringable, Type.int)] = Implements::True
      impls[Trait.new(stringable, Type.int)].should eq(Implements::True)
      impls[Trait.new(stringable, Type.int)].ok?.should be_true

      impls[Trait.new(stringable, Type.pointer(Type.int))] = Implements::True
      impls[Trait.new(stringable, Type.pointer(Type.int))].should eq(Implements::True)
      impls[Trait.new(stringable, Type.pointer(Type.int))].ok?.should be_true

      impls[Trait.new(stringable, Type.pointer(Type.int))] = Implements::False
      impls[Trait.new(stringable, Type.pointer(Type.int))].should eq(Implements::False)
      impls[Trait.new(stringable, Type.pointer(Type.int))].ok?.should be_false

      equatable_base_trait = TraitDef.new(loc, Mode::Let, "Equatable", Slice[TypeParameter.new(loc, "Self"), TypeParameter.new(loc, "T")])
      equatable = ->(type : Type) do
        Trait.new(equatable_base_trait, Slice[type])
      end
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])] = Implements::True
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])].should eq(Implements::True)
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])].ok?.should be_true
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(1)])]?.should be_nil

      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])] = Implements::False
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])].should eq(Implements::False)
      impls[Trait.new(equatable_base_trait, Slice[Type.pointer(Type.var(0)), Type.var(0)])].ok?.should be_false
    end
  end

  describe "#check_trait_implementations" do
    it "verifies basic trait implementations requiring zero or one type parameters, and up to two methods" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: program = <<-MISMO
        struct String
        struct Array[T]
          
        trait Trivial

        trait Stringable
          def string String

        extend Int is Trivial & Stringable
          def string String:
            "Int"

        trait Sequence[T] is Iterable[T]
          def get(index Nat) T

        trait Iterable[T]
          def iter Generator[T]

        struct Generator[T]
          var state T

        extend[T] Array[T] is Sequence[T]
          def get(index Nat) T:
            nil

          def iter Generator[T]:
            Generator(self)

            
        extend[T] Array[T] is Stringable
          def string String:
            "Array"        

        struct Array[T]
        MISMO
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.ready_to_validate_types = true
      # type_env.log.level = 
      type_env.check_trait_implementations
      trivial = type_env.traits["Trivial"]
      stringable = type_env.traits["Stringable"]
      sequence = type_env.traits["Sequence"]
      array = type_env.type_defs["Array"].as StructDef
      type_env.implementations[Trait.new(trivial, Type.int)]?.should eq(Implements::True)
      type_env.implementations[Trait.new(stringable, Type.int)]?.should eq(Implements::True)
      type_env.implementations[Trait.new(sequence, Slice[Type.adt(array, Slice[Type.var(0)]), Type.var(0)])]?.should eq(Implements::True)
      type_env.implementations[Trait.new(stringable, Type.adt(array, Slice[Type.var(0)]))]?.should eq(Implements::True)
      # type_env.trait_claims.each do |claim|
      #   print claim; print '\n'
      # end
      # type_env.implementations.each do |key, value|
      #   print "#{key[0]} is #{key[1]} => #{value}\n"
      # end
    end
    it "verifies more complex trait implementations" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: program = <<-MISMO
        trait Trivial

        trait Marker
          def is_marker Bool: true

        trait Equatable[T Equatable[Self]]
          def ==(other : T) -> Bool

        trait SelfEquatable
          def ==(other Self) -> Bool

        struct String

        extend Int is Equatable[Int] & SelfEquatable & Marker
          def ==(other : Int) -> Bool:
            self == other   
        MISMO
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.ready_to_validate_types = true
      type_env.check_trait_implementations
      trivial = type_env.traits["Trivial"]
      marker = type_env.traits["Marker"]
      equatable = type_env.traits["Equatable"]
      self_equatable = type_env.traits["SelfEquatable"]
      string_type_arg = Type.adt(type_env.type_defs["String"].as StructDef)
      type_env.implementations[Trait.new(trivial, Type.int)]?.should be_nil
      type_env.try_trait_implementation(Trait.new(trivial, Type.int)).should be_true
      type_env.implementations[Trait.new(trivial, Type.int)]?.should eq(Implements::True)
      type_env.implementations[Trait.new(marker, Type.int)]?.should eq(Implements::True)
      type_env.trait_implemented?(Trait.new(marker, Type.int)).should be_true
      type_env.trait_implemented?(Trait.new(equatable, Slice[Type.int, Type.int])).should be_true
      # type_env.implementations.each do |impl|
      #   print impl; print '\n'
      #   trait = impl[0][1]
      # end
      type_env.implementations[Trait.new(equatable, Slice[Type.int, Type.int])]?.should eq(Implements::True)
      type_env.implementations[Trait.new(self_equatable, Type.int)]?.should eq(Implements::True)

      type_env.trait_implemented?(Trait.new(trivial, string_type_arg)).should be_true
      type_env.trait_implemented?(Trait.new(marker, string_type_arg)).should be_false
      type_env.trait_implemented?(Trait.new(equatable, Slice[string_type_arg, string_type_arg])).should be_false
      type_env.trait_implemented?(Trait.new(self_equatable, string_type_arg)).should be_false
    end
    pending "detects when traits are implemented by generic traits" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: <<-MISMO
        trait Sequence[T]
          def get(index Nat) T

        extend[T] Array[T] is Sequence[T]
          def get(index Nat) T:
            nil

        extend Array[Int] is Sequence[Int]
        -- detecting this implementation will require a significantly more
        -- complex algorithm for the method_exists function

        struct Array[T]
        MISMO
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.check_trait_implementations
      sequence = type_env.traits["Sequence"]
      array = type_env.type_defs["Array"].as StructDef
      type_env.implementations[Trait.new(sequence, Type.adt(array, Slice[Type.var(0)]))]?.should eq(Implements::True)
    end
  end

  describe "#fill_out_type_info" do
    it "fills out type info for structs and enum variants" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: program = <<-MISMO
        struct String
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
           if opt is
              Some(t): Result.Ok(t)
              None: Result.Error(err)

        trait Stringable
           def string(self Self) -> String

        struct HasStringable[T Stringable]
           var item T

           def string -> String:
              "(" + item.string + ")"

        enum TryThis
           Bawal(HasStringable[Float])

        MISMO
      ).parse
      # type_env = TypeEnv.new(Logger.new)
      type_env = type_env("#fill_out_type_info fills out type info for structs and enum variants", program, nil)
      type_env.register_types_and_collect_items(items)
      type_env.register_functions
      type_env.eval_type_params_and_trait_claims(items)
      type_env.ready_to_validate_types = true
      type_env.check_trait_implementations
      # type_env.log.level = 
      type_env.fill_out_type_info(items)
      point = type_env.type_defs["IntPoint"].as StructDef
      point.fields.should eq([
        Field.new(loc, Binding::Var, "x", Type.int),
        Field.new(loc, Binding::Var, "y", Type.int),
      ])
      type_env.log.expect([
        "type Float does not satisfy trait Stringable[Float]"
      ])
    end
  end

  # NOTE: Built-ins are now included in the prelude
  # describe "#add_built_ins" do
  #   it "inserts a list of built-in functions" do
  #     type_env = TypeEnv.new(Logger.new)
  #     type_env.functions.size.should eq(0)
  #     type_env.add_built_ins
  #     type_env.functions.each.map(&.size).sum
  #   end
  # end

  describe "#type_check_program" do
    pending "detects when traits imply other traits, ie sub-trait inference" do
      items = parser(
        file_path: __FILE__, 
        line_offset: __LINE__ + 2,
        source: <<-MISMO
        trait Floatable
          def float -> Float

        trait Intable
          def int -> Int

        trait Numable
          def int -> Int
          def float -> Float

        def foo[T Numable](x T):
          bar(x)  -- getting this to typecheck will require sub-trait inference
                  -- otherwise the compiler requires explicit sub-trait annotations

        def bar[T Intable](x T):
          x.int
        
        MISMO
      ).parse
      type_env = TypeEnv.new(Logger.new)
      type_env.type_check_program(items)
      floatable = type_env.traits["Floatable"]
      intable = type_env.traits["Intable"]
      numable = type_env.traits["Numable"]
    end
  end
end