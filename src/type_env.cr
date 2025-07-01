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
  property trait_claims = [] of Ast::Extend  # {Ast::Type, Array(Ast::Type)}
  property ast_functions = {} of String => Array(Ast::Function)
  property functions = {} of String => Array(FunctionBase)
  property ready_to_validate_types = false

  def initialize(@log)
  end


  def type_check_program(items : Array(Ast::TopLevelItem))
    
    # first pass
    register_types_and_functions(items)

    # second pass: evaluate (without validating) function signatures
    # in order to use for checking trait implementations
    # ... or maybe I guess I should just use the ast functions for that?
    # ... then I have to keep them around for the whole type-checking process
    # I guess I'll keep them around then

    # second pass: check trait implementations
    check_trait_implementations

    # third pass: evaluate all type nodes of fields of structs and enum variants,
    #              and function signatures
    fill_out_type_info(items)

    add_built_ins

    # fourth pass: type check functions
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

  def register_types_and_functions(items : Array(Ast::TopLevelItem))
    # ast_functions = {} of String => Array(Ast::Function)
    # first pass: register named types
    log.info_descend(Location.zero, "register_types_and_functions") do
      items.each do |item|
        log.info(item.location, "register #{item.class} #{item.name}")
        case item
        when Ast::Function
          if ast_funcs = ast_functions[item.name]?
            ast_funcs << item
          else
            ast_functions[item.name] = [item]
          end
          if func_bases = functions[item.name]?
            func_bases << FunctionBase.new(item.location, item.name, item.type_params || [] of Ast::TypeParameter)
          else
            functions[item.name] = [FunctionBase.new(item.location, item.name, item.type_params || [] of Ast::TypeParameter)]
          end
        when Ast::Struct
          if item.name.in?(user_types)
            log.error(item.location, "type #{item.name} already defined")
          end
          user_types[item.name] = StructBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name, 
            item.type_params || [] of Ast::TypeParameter)
          if declared_traits = item.traits
            trait_claims << item.as_extension
          end
        when Ast::Enum
          if item.name.in?(user_types)
            log.error(item.location, "type #{item.name} already defined")
          end
          user_types[item.name] = EnumBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name, 
            item.type_params || [] of Ast::TypeParameter)
          if declared_traits = item.traits
            trait_claims << item.as_extension
          end
        when Ast::Trait
          if item.name.in?(traits)
            log.error(item.location, "trait #{item.name} already defined")
          end
          traits[item.name] = TraitBase.new(
            item.location, 
            item.convention || Mode::Let, 
            item.name, 
            item.type_params || [] of Ast::TypeParameter, 
            item.methods
          )
        when Ast::Extend
          if item.traits
            trait_claims << item
          end
        else
          raise "TypeEnv#register_types_and_functions: unknown top-level item: #{item}"
        end
      end
    end
    # ast_functions
  end

  def check_trait_implementations
    # first, assume that all claims are valid, contradictions will be caught later
    trait_claims.each do |extension|
      context = TypeContext.new(self, extension.type_params)
      type = context.eval(extension.type)
      extension.traits.try &.each do |trait_node|
        trait = context.eval_trait(trait_node)
        implementations[{type, trait}] = Implements::Calculating
      end
    end

    @ready_to_validate_types = true

    # now reevaluate all claims, given the assumption, and check for inconsistencies
    trait_claims.each do |extension|
      context = TypeContext.new(self, extension.type_params)
      type = context.eval(extension.type)
      extension.traits.try &.each do |trait_node|
        trait = context.eval_trait(trait_node)
        unless context._type_implements_trait?(type, trait)
          log.error(extension.location, "type #{type} does not implement trait #{trait}")
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
      case item
      when Ast::Struct
        struct_base = user_types[item.name].as(StructBase)
        item.fields.each do |field|
          field_type = context.eval(field.type)
          struct_base.fields << Field.new(field.location, field.binding, field.name, field_type)
          upsert(FunctionBase.new(
            field.location, 
            field.name, 
            item.type_params || [] of Ast::TypeParameter, 
            [Parameter.new(field.location, Mode::Let, "self", Type.struct(struct_base))], 
            field_type))
        end
        # add constructor
        upsert(FunctionBase.new(
          item.location, 
          item.name, 
          item.type_params || [] of Ast::TypeParameter,
          struct_base.fields.map do |field|
            Parameter.new(field.location, field.binding.to_mode(Mode::Move), field.name, field.type)
          end,
          Type.struct(struct_base)))
      when Ast::Enum
        enum_base = user_types[item.name].as(EnumBase)
        item.variants.each do |variant|
          enum_base.variants << 
            Variant.new(variant.location, variant.name, variant.fields.try &.map { |field_name, field_type| 
              Field.new(field_type.location, Binding::Var, field_name, context.eval(field_type)) 
            } || [] of Field)
        end
      when Ast::Function
        function_base = get_function(item.name, item.location)
        item.parameters.try &.each do |param|
          type = context.eval(param.type)
          mode = param.convention || type.mode
          function_base.parameters << Parameter.new(param.location, mode, param.name, type)
        end
        if ret_type = item.return_type
          function_base.return_type = context.eval(ret_type)
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
    [Ast::TypeParameter.new(Location.zero, "T")], 
    [Parameter.new(Location.zero, Mode::Let, "value", Type::Var.new(1))],
    Type.nil),
  FunctionBase.new(Location.zero, 
    "+",
    [] of Ast::TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int),
  FunctionBase.new(Location.zero, 
    "-",
    [] of Ast::TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int),
  FunctionBase.new(Location.zero, 
    "*",
    [] of Ast::TypeParameter,
    [Parameter.new(Location.zero, Mode::Let, "left", Type.int),
     Parameter.new(Location.zero, Mode::Let, "right", Type.int)],
    Type.int)
]
