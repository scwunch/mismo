require "../type_checker/hir_nodes"
require "../ast/ast_nodes"
require "../type_checker/types"
require "../type_checker/type_checker"
# require "../prelude/builtins"

# This enum is used instead of simple booleans for checking trait implementations
# There are cases in which checking that a type implements a trait requires its own
# implementation to be valid.  This is circular, so we use `Calculating` to break
# the cycle and allow the type to be validated.
enum Implements
  True
  Calculating
  False
  def ok?
    self != False
  end
end

class TypeEnv
  property log : Logger
  property type_defs = {} of String => TypeDefinition
  property traits = {} of String => TraitDef
  property implementations : ::Hash(Trait, Implements) = {} of Trait => Implements
  property trait_claims = [] of TraitClaim
  # property ast_functions = {} of String => Array(Ast::Function)
  # property external_functions = [] of Ast::ExternalFunction
  property function_defs = {} of String => Array(FunctionDef)
  property functions = {} of String => Array(Function)
  property ready_to_validate_types = false
  property eval_depth = 0

  def initialize(@log)
  end

  def type_check_program(items : Array(Ast::TopLevelItem))
    declaration_collection(items)
    type_check_functions
  end

  def declaration_collection(items : Array(Ast::TopLevelItem))
    
    # first pass
    register_types_and_collect_items(items)

    set_string_and_array_bases

    # second pass: evaluate (without validating) all top-level type nodes; this
    # includes function signatures, collecting trait claims, and all type parameters.
    # This info will be used to check trait implementations
    # register_external_functions
    # register_functions
    eval_type_params_and_trait_claims(items)

    @ready_to_validate_types = true

    # third pass: check trait implementation consistency
    check_trait_implementations

    # fourth pass: evaluate all type nodes of fields of structs and enum variants
    fill_out_type_info(items)

    # add_built_ins
    # NOTE: Built-ins are now included in the prelude
  end
  
  def type_check_functions_old
    # fifth pass: type check functions
    ast_functions.each do |name, ast_funcs| 
      # name, ast_funcs = ast_funcs
      ast_funcs.each_with_index do |f, i|
        func_base = function_defs[name][i]
        # if params = f.parameters
        #   func_base.parameters = params.map { |p| Parameter.new(p.location, p.name, eval(p.type)) }
        # end
        # if ret_type = f.return_type
        #   func_base.return_type = eval(ret_type)
        # end
        TypeContext.type_check_function(self, func_base, f.body)
      end
    end
  end

  def type_check_functions
    mains = function_defs["main"]?
    unless mains
      log.error(Location.zero, "#{__FILE__}:#{__LINE__} no main function")
      return Hash(String, Array(Function)).new
    end
    unless mains.size == 1
      log.error(Location.zero, "#{__FILE__}:#{__LINE__} multiple main functions")
    end
    specializer = Specializer.new(self, mains.first)
    specializer.run
    specializer.functions
  end

  # For each Ast struct, enum, and trait, construct its complementary HIR item in 
  # the type environment.  Fields, parameters, and constraints are not yet included.
  # Functions and trait_claims are collected for processing in later steps.
  def register_types_and_collect_items(items : Array(Ast::TopLevelItem))
    # ast_functions = {} of String => Array(Ast::Function)
    # first pass: register named types
    log.info_descend(Location.zero, "register_types_and_collect_items") do
      items.each do |item|
        case item
        when Ast::Function
          # only collecting ast functions for now, `BaseFunction`s will be registered in 
          # next step, because parameter types require all types to be registered first.
          # if ast_funcs = ast_functions[item.name]?
          #   ast_funcs << item
          # else
          #   ast_functions[item.name] = [item]
          # end
        when Ast::ExternalFunction
          # external_functions << item
        when Ast::Struct
          log.info(item.location, "register struct #{item.name}")
          if item.name.in?(type_defs)
            log.error(item.location, "E#{__LINE__} type #{item.name} already defined")
          end
          type_defs[item.name] = StructDef.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.map { |tp| TypeParameter.new(tp.location, tp.name) }
          )
          # if declared_traits = item.traits
          #   trait_claims << item.as_extension
          # end
        when Ast::Enum
          log.info(item.location, "register enum #{item.name}")
          if item.name.in?(type_defs)
            log.error(item.location, "E#{__LINE__} type #{item.name} already defined")
          end
          type_defs[item.name] = EnumDef.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.map { |tp| TypeParameter.new(tp.location, tp.name) }
          )
          # if declared_traits = item.traits
          #   trait_claims << item.as_extension
          # end
        when Ast::Trait
          log.info(item.location, "register trait #{item.name}")
          if item.name.in?(traits)
            log.error(item.location, "E#{__LINE__} trait #{item.name} already defined")
          end
          # type_params = Slice(TypeParameter).new(item.type_params.size + 1) do |i|
          #   if i == 0
          #     TypeParameter.new(item.location, "Self")
          #   else
          #     TypeParameter.new(item.type_params[i - 1].location, item.type_params[i - 1].name)
          #   end
          # end
          # p! type_params
          traits[item.name] = TraitDef.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.map { |tp| TypeParameter.new(tp.location, tp.name) },
            item.methods
          )
        when Ast::Extend
          # if item.traits
          #   trait_claims << item
          # end
        else
          raise "TypeEnv#register_types_and_collect_items: unknown top-level item: #{item}"
        end
      end
    end
  end

  def set_string_and_array_bases
    case string_base = type_defs["String"]?
    when StructDef
      Hir::String.set_base(string_base)
    when EnumDef
      log.warning(Location.zero, "E#{__LINE__} String is an enum, not a struct")
    else
      log.warning(Location.zero, "E#{__LINE__} String is not defined")
    end
    case array_base = type_defs["Array"]?
    when StructDef
      Hir::Array.set_base(array_base)
    when EnumDef
      log.warning(Location.zero, "E#{__LINE__} Array is an enum, not a struct")
    else
      log.warning(Location.zero, "E#{__LINE__} Array is not defined")
    end
  end

  def eval_type_params_and_trait_claims(items : Array(Ast::TopLevelItem))
    log.info_descend(Location.zero, "evaluate type params and trait claims") do
      items.each do |item|
        log.debug_descend(item.location, "eval_type_params_and_trait_claims #{item}") do
          case item
          when Ast::Function, Ast::ExternalFunction
            # type parameters already evaluated in `#register_functions`
            function = new_function(item)
            function.set_as_extern if item.is_a?(Ast::ExternalFunction)
            if overloads = function_defs[item.name]?
              overloads << function
            else
              function_defs[item.name] = [function]
            end
            
          when Ast::Struct, Ast::Enum
            context = TypeContext.new(self, item.type_params)
            log.debug(item.location, "context: #{context}")
            base_type = type_defs[item.name]
            base_type.type_params = context.type_parameters
            type = Type.adt(base_type, context.type_args)
            item.traits.try &.each do |trait|
              trait_claims << TraitClaim.new(
                item.location, 
                context.type_parameters, 
                context.eval_trait(trait, type)
              )
            end
          when Ast::Trait
            # context = TypeContext.new(self, Slice[Ast::TypeParameter.new(item.location, "Self")] + item.type_params)
            context = TypeContext.new(self, item.type_params)
            log.debug(item.location, "context: #{context}")
            trait = traits[item.name].as(TraitDef)
            trait.type_params = context.type_parameters
          when Ast::Extend
            if claimed_traits = item.traits
              context = TypeContext.new(self, item.type_params)
              log.debug(item.location, "context: #{context}")
              type = context.eval(item.type)
              claimed_traits.each do |trait|
                trait_claims << TraitClaim.new(
                  item.location, 
                  context.type_parameters, 
                  context.eval_trait(trait, type) 
                )
              end
            end
          end
        end
      end
    end
  end

  def register_external_functions
    log.info_descend(Location.zero, "Register #{external_functions.size} external functions") do
      external_functions.each do |func|
        log.debug_descend(func.location, "register #{func.name}#{func.signature}") do
          if function_defs.includes?(func.name)
            log.error(func.location, "E#{__LINE__} duplicate externally implemented function: #{func.name}; overwriting.")
          end
          register_function(
            func, 
            function_defs[func.name] = [] of FunctionDef
          )
            .set_as_extern
        end
      end
    end
  end

  # Iterate ast_functions (a tree structure created in the previous step)
  # and register each function in the type environment.  Parameter types 
  # and return types are evaluated, but trait bounds are not checked yet.
  def register_functions
    log.info_descend(Location.zero, "Register all functions (#{ast_functions.size})") do
      ast_functions.each do |name, overloads|
        if function_defs.includes?(name)
          log.warning(overloads.first.location, "E#{__LINE__} overwriting externally implemented function: #{name}")
        end
        func_bases = function_defs[name] = [] of FunctionDef
        overloads.each do |ast_func|
          register_function(ast_func, func_bases)
          # context = TypeContext.new(self, ast_func.type_params)
          # log.debug_descend(ast_func.location, "register #{ast_func.name}#{ast_func.signature} (context=#{context})") do
          #   function = FunctionDef.new(
          #     location: ast_func.location, 
          #     name: ast_func.name, 
          #     type_params: context.type_parameters,
          #     parameters: ast_func.parameters.try &.map do |param|
          #         type = context.eval(param.type)
          #         mode = param.convention || type.mode
          #         Parameter.new(param.location, mode, param.name, type)
          #       end || [] of Parameter,
          #     return_type: if ret_type = ast_func.return_type
          #         context.eval(ret_type)
          #       else
          #         Type.nil
          #       end
          #   )
          #   log.debug(ast_func.location, "Function registered: #{function}")
          #   func_bases << function
          # end
        end
      end
    end
  end

  def register_function(ast_func : Ast::Function | Ast::ExternalFunction, func_bases : Array(FunctionDef))
    func = new_function(ast_func)
    func_bases << func
    func
  end

  def new_function(ast_func : Ast::Function | Ast::ExternalFunction)
    context = TypeContext.new(self, ast_func.type_params)
    log.debug_descend(ast_func.location, "register #{ast_func.name}#{ast_func.signature} (context=#{context})") do
      function = FunctionDef.new(
        location: ast_func.location, 
        name: ast_func.name, 
        type_params: context.type_parameters,
        parameters: ast_func.parameters.try &.map do |param|
            type = context.eval(param.type)
            mode = param.convention || type.mode
            Parameter.new(param.location, mode, param.name, type)
          end || [] of Parameter,
        return_type: if ret_type = ast_func.return_type
            context.eval(ret_type)
          else
            Type.nil
          end,
        body: ast_func.body? || Ast::Block.empty
      )
      log.debug(ast_func.location, "Function registered: #{function}")
      function
    end
  end

  # For each "Type is Trait" ("extend" clauses and traits put directly on a type),
  # check to make sure the trait itself is valid and that the required methods
  # actually exist.
  def check_trait_implementations
    if !ready_to_validate_types
      log.warning(Location.zero, "#{__FILE__}:#{__LINE__} TypeEnv#check_trait_implementations called, but TypeEnv is not ready to validate types")
    end
    log.info_descend(Location.zero, "Check trait implementations (#{trait_claims.size})") do
      trait_claims.each do |extension|
        context = TypeContext.new(self, extension.type_params)
        # ummm, what is this context supposed to do???
        log.info_descend(extension.location, "Check #{extension.trait} implemented (context=#{context})") do
          unless try_trait_implementation(extension.trait)
            log.error(extension.location, "E#{__LINE__} #{extension.trait} not implemented")
          end
        end
      end
    end
    # TODO: support trait inheritance
  end

  # Check if each required method of a trait is actually implemented.  Cache results.
  def trait_implemented?(trait : Trait) : Bool
    log.debug_descend(Location.zero, "#trait_implemented? #{trait}") do
      return true if implementations[trait]?.try &.ok?
      trait.base.methods.all? do |method|
        # log.error(method.location, "you have to add the Self type parameter to the method")
        # raise "you have to add the Self type parameter to the method"
        context = TypeContext.new(self, method.type_params, trait.type_args)
        context.method_implemented?(method)
      end
    end
  end

  # Check if each required method of a trait is actually implemented. 
  # If a trait method is missing but has a default implementation, add that 
  # implementation.  Otherwise raise an error.
  # Cache results.
  def try_trait_implementation(trait : Trait)
    log.debug_descend(Location.zero, "#try_trait_implementation #{trait}") do
      case implementations[trait]?
      in Implements::Calculating
        true
      in Implements::True
        true
      in Implements::False
        false
      in Nil
        implementations[trait] = Implements::Calculating
        impl = true
        trait.base.methods.each do |method|
          context = TypeContext.new(self, method.type_params, trait.type_args)
          if !context.try_method_implementation(method)
            impl = false
          end
        end
        implementations[trait] = impl ? Implements::True : Implements::False
        impl
      end
    end
  end

  # evaluate all type nodes for the fields of structs and of enum variants
  # also re-evaluate the type nodes of function parameters and return types
  def fill_out_type_info(items : Array(Ast::TopLevelItem))
    log.info_descend(Location.zero, "Evaluate type nodes for fields and function parameters") do
      items.each do |item|
        context = TypeContext.new(self, item.type_params)
        log.info_descend(item.location, "Fill out type info for #{item} (context=#{context})") do
          case item
          when Ast::Struct
            struct_base = type_defs[item.name].as(StructDef)
            this = Type::Adt.new(struct_base, context.type_args)
            item.fields.each_with_index do |ast_field, i|
              # evaluate field
              field_type = context.eval(ast_field.type)
              field = Field.new(ast_field.location, ast_field.binding, ast_field.name, field_type)
              struct_base.fields << field

              # add field-access wrapper function
              upsert(FunctionDef.new(
                field.location, 
                field.name, 
                context.type_parameters, 
                [Parameter.new(field.location, Mode::Let, "self", this)], 
                Mode::Mut,
                field_type,
                body: Ast::FieldAccess.new(
                  field.location, 
                  Ast::Identifier.new(field.location, "self"), 
                  field.name
                )
              ))
              #   body: 
              #     Hir.accessfield(
              #       field.location, 
              #       Cell.new(Hir.identifier(field.location, "self", Binding::Let, this)), 
              #       field
              #     )
              # ))
            end
            # generate constructor
            upsert(FunctionDef.new(
              item.location, 
              item.name, 
              context.type_parameters,
              parameters: struct_base.fields.map do |field|
                Parameter.new(field.location, field.binding.to_mode(Mode::Move), field.name, field.type)
              end,
              return_type: Type.adt(struct_base, context.type_args),
              body: Ast::Constructor.new(
                item.location,
                Ast::Type.new(item.location, item.name, Ast.to_type_args(item.type_params)),
                struct_base.fields.map do |field|
                  Ast::Identifier.new(field.location, field.name).as(Ast::Expr)
                end
              )
            ))
            #   body: 
            #     Hir.constructor(
            #       item.location,
            #       this,
            #       struct_base.fields.map do |field|
            #         Hir.identifier(field.location, field.name, field.binding, field.type)
            #       end
            #     )
            #   )
            # )
          when Ast::Enum
            enum_base = type_defs[item.name].as(EnumDef)
            item.variants.each do |variant|
              # evaluate variant
              # discriminant = Val.new(enum_base.variants.size)
              variant = 
                Variant.new(variant.location, variant.name, variant.fields.try &.map { |field_name, field_type| 
                  Field.new(field_type.location, Binding::Var, field_name, context.eval(field_type)) 
                } || [] of Field)
              enum_base.variants << variant

              # generate constructor
              constructor_args = [Ast::Int.new(variant.location, enum_base.variants.size.to_i128).as(Ast::Expr)]
              variant.fields.each do |field|
                constructor_args << Ast::Identifier.new(field.location, field.name)
              end
              constructor_ast = Ast::Constructor.new(
                variant.location,
                Ast::Type.new(variant.location, item.name, Ast.to_type_args(item.type_params)),
                constructor_args
              )
              log.debug(variant.location, "Constructor for #{item.name}.#{variant.name}: #{constructor_ast}")
              upsert(FunctionDef.new(
                location: variant.location, 
                name: "#{item.name}.#{variant.name}", 
                type_params: context.type_parameters,
                parameters: variant.fields.map do |field|
                  Parameter.new(field.location, Mode::Move, field.name, field.type)
                end || [] of Parameter,
                return_type: Type.adt(enum_base, context.type_args),
                body: constructor_ast
              ))
              log.debug(variant.location, "Verify constructor function added: #{function_defs["#{item.name}.#{variant.name}"]}")
                # if fields = variant.fields
                #   raise "I guess you should make a constructor here..."
                #   # ->(interpreter : Interpreter) {
                #   #   Val.new(Slice(Val).new(fields.size + 1) do |i|
                #   #     if i == 0
                #   #       discriminant
                #   #     else
                #   #       interpreter.frame.variables[fields[i-1].name]
                #   #     end
                #   #   end)
                #   # }
                # else
                #   raise "I guess you should make a constructor here..."
                #   # ->(interpreter : Interpreter) {
                #   #   Val.new(Slice[discriminant])
                #   # }
                # end
            end
          when Ast::Function, Ast::ExternalFunction
            # merely validate the types of the parameters that are already there
            function_base = get_function(item.name, item.location)
            item.parameters.try &.each do |param|
              context.eval(param.type)
            end
            if ret_type = item.return_type
              context.eval(ret_type)
            end
          when Ast::Trait, Ast::Import, Ast::Extend
            # pass
          else
            log.error(item.location, "E#{__LINE__} unknown item type: #{item}")
          end
        end
      end
    end
  end

  def get_function(name : String, loc : Location)
    function_defs[name].find { |f| f.location == loc } ||
      raise "TypeEnv#get_function: unknown function: #{name} @ #{loc}"
  end

  # NOTE: Built-ins are now included in the prelude
  # def add_built_ins
  #   BUILTINS.each do |func|
  #     upsert(func)
  #   end
  # end

  def upsert(func : FunctionDef)
    if overloads = function_defs[func.name]?
      overloads << func
    else
      function_defs[func.name] = [func]
    end
  end
end

struct TraitClaim < IrNode
  getter location : Location
  getter type_params : Slice(TypeParameter)
  getter trait : Trait
  def initialize(@location : Location, @type_params : Slice(TypeParameter), @trait : Trait)
  end
  def initialize(@location : Location, @type_params : Slice(TypeParameter), type : Type, trait : Trait)
    @trait = Trait.new(trait.base, Slice[type] + trait.type_args)
  end
  def to_s(io : IO)
    io << "trait claim: [#{type_params.join(", ")}] #{trait}" 
  end
end