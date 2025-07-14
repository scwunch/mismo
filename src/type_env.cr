require "./hir_nodes"
require "./ast_nodes"
require "./types"
require "./type_checker"

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
  property user_types = {} of String => TypeInfo
  property traits = {} of String => TraitBase
  property implementations : ::Hash({Type, Trait}, Implements) = {} of {Type, Trait} => Implements
  property trait_claims = [] of TraitClaim
  property ast_functions = {} of String => Array(Ast::Function)
  property functions = {} of String => Array(FunctionBase)
  property ready_to_validate_types = false
  property eval_depth = 0

  def initialize(@log)
  end


  def type_check_program(items : Array(Ast::TopLevelItem))
    
    # first pass
    register_types_and_collect_items(items)

    # second pass: evaluate (without validating) all top-level type nodes; this
    # includes function signatures, collecting trait claims, and all type parameters.
    # This info will be used to check trait implementations
    register_functions
    eval_type_params_and_trait_claims(items)

    # third pass: check trait implementation consistency
    check_trait_implementations
    # NOTE: this step sets @ready_to_validate_types = true

    # fourth pass: evaluate all type nodes of fields of structs and enum variants
    fill_out_type_info(items)

    add_built_ins

    # fifth pass: type check functions
    ast_functions.each do |name, ast_funcs| 
      # name, ast_funcs = ast_funcs
      ast_funcs.each_with_index do |f, i|
        func_base = functions[name][i]
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
          if ast_funcs = ast_functions[item.name]?
            ast_funcs << item
          else
            ast_functions[item.name] = [item]
          end
        when Ast::Struct
          log.info(item.location, "register struct #{item.name}")
          if item.name.in?(user_types)
            log.error(item.location, "type #{item.name} already defined")
          end
          type_params = item.type_params.try &.map { |tp| 
              TypeParameter.new(tp.location, tp.name) 
            } || [] of TypeParameter
          user_types[item.name] = StructBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.try &.map { |tp| TypeParameter.new(tp.location, tp.name) } || [] of TypeParameter
          )
          # if declared_traits = item.traits
          #   trait_claims << item.as_extension
          # end
        when Ast::Enum
          log.info(item.location, "register enum #{item.name}")
          if item.name.in?(user_types)
            log.error(item.location, "type #{item.name} already defined")
          end
          type_params = item.type_params.try &.map { |tp| 
              TypeParameter.new(tp.location, tp.name) 
            } || [] of TypeParameter
          user_types[item.name] = EnumBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.try &.map { |tp| TypeParameter.new(tp.location, tp.name) } || [] of TypeParameter
          )
          # if declared_traits = item.traits
          #   trait_claims << item.as_extension
          # end
        when Ast::Trait
          log.info(item.location, "register trait #{item.name}")
          if item.name.in?(traits)
            log.error(item.location, "trait #{item.name} already defined")
          end
          type_params = item.type_params.try &.map { |tp| 
              TypeParameter.new(tp.location, tp.name) 
            } || [] of TypeParameter
          traits[item.name] = TraitBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name,
            item.type_params.try &.map { |tp| TypeParameter.new(tp.location, tp.name) } || [] of TypeParameter,
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

  def eval_type_params_and_trait_claims(items : Array(Ast::TopLevelItem))
    log.info_descend(Location.zero, "evaluate type params and trait claims") do
      items.each do |item|
        context = TypeContext.new(self, item.type_params)
        log.debug_descend(item.location, "eval_type_params_and_trait_claims #{item.name} (context=#{context})") do
          case item
          when Ast::Function
          when Ast::Struct, Ast::Enum
            type = user_types[item.name]
            type.type_params = context.type_parameters
            if claimed_traits = item.traits
              trait_claims << TraitClaim.new(
                item.location, 
                context.type_parameters, 
                Type.adt(type, context.type_params_as_args), 
                claimed_traits.map { |t| context.eval_trait(t) }
              )
            end
          when Ast::Trait
            trait = traits[item.name].as(TraitBase)
            trait.type_params = context.type_parameters
          when Ast::Extend
            if claimed_traits = item.traits
              trait_claims << TraitClaim.new(
                item.location, 
                context.type_parameters, 
                context.eval(item.type), 
                claimed_traits.map { |t| context.eval_trait(t) }
              )
            end
          end
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
        func_bases = functions[name] = [] of FunctionBase
        overloads.each do |ast_func|
          context = TypeContext.new(self, ast_func.type_params)
          log.debug_descend(ast_func.location, "register #{ast_func.name}#{ast_func.signature} (context=#{context})") do
            function = FunctionBase.new(
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
                end
            )
            log.debug(ast_func.location, "Function registered: #{function}")
            func_bases << function
          end
        end
      end
    end
  end

  # For each "Type is Trait" ("extend" clauses and traits put directly on a type),
  # check to make sure the trait itself is valid and that the required methods
  # actually exist.
  # This step sets @ready_to_validate_types = true
  def check_trait_implementations
    log.info_descend(Location.zero, "Check trait implementations (#{trait_claims.size})") do
      # first, assume that all claims are valid, contradictions will be caught later
      trait_claims.each do |extension|
        log.debug_descend(extension.location, "Assume #{extension.type} implements #{extension.traits}") do
          extension.traits.try &.each do |trait|
            implementations[{extension.type, trait}] = Implements::Calculating
          end
        end
      end

      @ready_to_validate_types = true

      # now reevaluate all claims, given the assumption, and check for inconsistencies
      trait_claims.each do |extension|
        context = TypeContext.new(self, extension.type_params)
        log.info_descend(extension.location, "Check #{extension.type} implements #{extension.traits} (context=#{context})") do
          extension.traits.try &.each do |trait|
            log.debug_descend(extension.location, "Check #{extension.type} implements #{trait}") do
              unless context._type_implements_trait?(extension.type, trait)
                log.error(extension.location, "type #{extension.type} does not implement trait #{trait}")
              end
            end
          end
        end
      end
    end
    # TODO: support trait inheritance
  end

  # evaluate all type nodes for the fields of structs and of enum variants
  # also re-evaluate the type nodes of function parameters and return types
  def fill_out_type_info(items : Array(Ast::TopLevelItem))
    items.each do |item|
      context = TypeContext.new(self, item.type_params)
      log.debug_descend(item.location, "Fill out type info for #{item.name} (context=#{context})") do
        case item
        when Ast::Struct
          struct_base = user_types[item.name].as(StructBase)
          item.fields.each do |field|
            field_type = context.eval(field.type)
            struct_base.fields << Field.new(field.location, field.binding, field.name, field_type)
            upsert(FunctionBase.new(
              field.location, 
              field.name, 
              context.type_parameters, 
              [Parameter.new(field.location, Mode::Let, "self", Type.struct(struct_base))], 
              field_type
            ))
          end
          # add constructor
          upsert(FunctionBase.new(
            item.location, 
            item.name, 
            context.type_parameters,
            struct_base.fields.map do |field|
              Parameter.new(field.location, field.binding.to_mode(Mode::Move), field.name, field.type)
            end,
            Type.struct(struct_base)
          ))
        when Ast::Enum
          enum_base = user_types[item.name].as(EnumBase)
          item.variants.each do |variant|
            enum_base.variants << 
              Variant.new(variant.location, variant.name, variant.fields.try &.map { |field_name, field_type| 
                Field.new(field_type.location, Binding::Var, field_name, context.eval(field_type)) 
              } || [] of Field)
          end
        when Ast::Function
          # merely validate the types of the parameters that are already there
          function_base = get_function(item.name, item.location)
          item.parameters.try &.each do |param|
            context.eval(param.type)
          end
          if ret_type = item.return_type
            context.eval(ret_type)
          end
        end
      end
    end
  end

  def get_function(name : String, loc : Location)
    functions[name].find { |f| f.location == loc } ||
      raise "TypeEnv#get_function: unknown function: #{name} @ #{loc}"
  end

  def add_built_ins
    BUILTINS.each do |func|
      upsert(func)
    end
  end

  def upsert(func : FunctionBase)
    if overloads = functions[func.name]?
      overloads << func
    else
      functions[func.name] = [func]
    end
  end
end

BUILTINS = [
  FunctionBase.new(Location.zero, 
    "print", 
    [TypeParameter.new(Location.zero, "T")], 
    [Parameter.new(Location.zero, Mode::Let, "value", Type.var(1))],
    Type.nil),
  FunctionBase.new(Location.zero, 
    "+",
    [] of TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int),
  FunctionBase.new(Location.zero, 
    "-",
    [] of TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int),
  FunctionBase.new(Location.zero, 
    "*",
    [] of TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int)
]

struct TraitClaim < IrNode
  getter location : Location
  getter type_params : Array(TypeParameter)
  getter type : Type
  getter traits : Array(Trait)
  def initialize(@location : Location, @type_params : Array(TypeParameter), @type : Type, @traits : Array(Trait))
  end
  def to_s(io : IO)
    io << "trait claim: #{type} implements #{traits.join(", ")}" 
  end
end