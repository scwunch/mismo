require "./type_env"
require "./logger"

# Type inference context
class TypeContext
  getter env : TypeEnv
  property scope = TypeScope.new
  getter generics = {} of String => Type::Var
  getter bindings = {} of Type::Var => Type
  getter unifier = TypeUnifier.new

  def initialize(@env, type_params : Array(Ast::TypeParameter)? = nil)
    if type_params
      type_params.each do |type_param|
        generics[type_param.name] = Type::Var.new(unifier.fresh_type_var.id)
      end
    end
  end

  def log : Logger
    env.log
  end

  def self.type_check_function(env : TypeEnv, function : FunctionBase, statements : Array(Ast::Expr))
    ctx = TypeContext.new(env, function.type_params)
    # init scope with parameters
    function.parameters.each do |param|
      ctx.scope[param.name] = Variable.new(param.location, param.mode.to_binding, param.name, param.type)
    end
    
    # check body, statement-by-statement
    function.body = statements.map { |e| ctx.type_check(e) }
    
    # check return type
    unless function.return_type == Type.nil
      begin
        ctx.unify(function.body.last.type, function.return_type)
      rescue failure
        if msg = failure.message
          ctx.log.error(function.body.last.location, msg)
        end
        ctx.log.error(function.body.last.location, "Expected return type: #{function.return_type}; Actual return type: #{function.body.last.type}")
      end
    end
  end

  def eval(type_node : Ast::Type) : Type
    # type_args = (type_node.type_args || [] of Ast::Type).map { |t| eval(t) }
    case type_node.name
    when "Never"
      eval_type_args("Primitive type Never", nil, type_node.type_args)
      Type.never
    when "Nil" 
      eval_type_args("Primitive type Nil", nil, type_node.type_args)
      Type.nil
    when "Bool" 
      eval_type_args("Primitive type Bool", nil, type_node.type_args)
      Type.bool
    when "Int" 
      eval_type_args("Primitive type Int", nil, type_node.type_args)
      Type.int
    when "Float" 
      eval_type_args("Primitive type Float", nil, type_node.type_args)
      Type.float
    when "String" 
      eval_type_args("String", nil, type_node.type_args)
      Type.string
    when "Array"
      type_args = eval_type_args("Array", [Ast::TypeParameter.new(Location.zero, "T")], type_node.type_args)
      Type.array(type_args[0]? || Type.never)
    when "Tuple"
      Type.tuple((type_node.type_args || [] of Ast::Type).map { |t| eval(t) })
    # when "Function" then Type.function(*type_node.types.map { |t| eval(t) })
    else
      if var = generics[type_node.name]?
        eval_type_args("A type variable (#{type_node.name})", nil, type_node.type_args)
        return bindings[var]? || var
      end
      case base = env.user_types[type_node.name]?
      when StructBase
        Type.struct(
          base, 
          eval_type_args("struct #{base.name}", base.type_params, type_node.type_args)
        )
      when EnumBase
        Type.enum(
          base, 
          eval_type_args("enum #{base.name}", base.type_params, type_node.type_args)
        )
      else
        raise "#eval: unknown type: #{type_node.name}"
      end
    end
  rescue err
    log.error(type_node.location, err.message || "TYPE ERROR @ {{__line}}")
    Type.never
  end

  def eval_type_args(
    type_name : String, 
    type_params : Array(Ast::TypeParameter)?, 
    type_args : Array(Ast::Type)?) : Array(Type)
    case {type_params, type_args}
    in {nil, nil}
      [] of Type
    in {nil, Array(Ast::Type)}
      raise "#{type_name} does not expect type arguments, but #{type_args.size} were provided." unless type_args.size == 0
      [] of Type
    in {Array(Ast::TypeParameter), nil}
      raise "#{type_name} expects #{type_params.size} type arguments, but none were provided." unless type_params.size == 0
      [] of Type
    in {Array(Ast::TypeParameter), Array(Ast::Type)}
      if type_args.size != type_params.size
        raise "#{type_name} expects #{type_params.size} type arguments, but #{type_args.size} were provided."
      end
      type_params.zip(type_args).map do |type_param, type_arg|
        eval(type_arg, type_param)
      end
    end
  end

  # def validate_type_args(type : Type, type_args : Array(Ast::Type)?) : String?
    # case type
    # when .primitive?
    #   if type_args
    #     "Primitive types do not take type arguments."
    #   end
    # when Type::Var
    #   if type_args
    #     "Type variables cannot take type arguments."
    #   end
    # when Type::Array
    #   unless type_args && type_args.size == 1
    #     "TypeEnv#validate_type_args: expected 1 type arg for Array, got #{type_args.size}"
    #   end
    # when Type::Tuple
    #   unless type_args
    #     "Tuple must take type arguments"
    #   end
    # when Type::Struct, Type::Enum
    #   base_type = user_types[type.name]
    #   if type_args
    #     if type_args.size != base_type.type_params.size
    #       return "TypeEnv#validate_type_args: expected #{base_type.type_params.size} type args for #{type.name}, got #{type_args.size}"
    #     end
    #   elsif base_type.type_params.size != 0
    #     return "TypeEnv#validate_type_args: expected #{base_type.type_params.size} type args for #{type.name}, got 0"
    #   end

    #   type.type_params.zip(type_args).each do |type_param, type_arg|
    #     type_satisfies_constraints(type_arg, type_param)
    #   end
    # else
    #   "TypeEnv#validate_type_args: unknown type: #{type}"
    # end
  # end

  def type_satisfies_constraints(type : Type, type_param : Ast::TypeParameter) : String?
    if incl = type_param.constraints.includes
      incl.each do |trait|
        unless type_implements_trait?(type, eval_trait(trait))
          return "TypeEnv#type_satisfies_constraints: type #{type} does not satisfy trait #{trait}"
        end
      end
    end
    if excl = type_param.constraints.excludes
      excl.each do |trait|
        if type_implements_trait?(type, eval_trait(trait))
          return "TypeEnv#type_satisfies_constraints: type #{type} satisfies excluded trait #{trait}"
        end
      end
    end
    nil
  end

  def eval(type_node : Ast::Type, constraints : Ast::TypeParameter)
    type = eval(type_node)
    if env.ready_to_validate_types && (error = type_satisfies_constraints(type, constraints))
      log.error(type_node.location, error)
    end
    type
  end

  def type_implements_trait?(type : Type, trait : Trait)
    env.implementations.fetch({type, trait}) do
      env.implementations[{type, trait}] = Implements::Calculating
      return _type_implements_trait?(type, trait)
    end.ok?
  end

  def _type_implements_trait?(type : Type, trait : Trait) : Bool
    (env.implementations[{type, trait}] = 
      if trait.base.methods.all? { |method| method.body || type_has_method?(type, method) }
        Implements::True
      else
        Implements::False
      end
    ).ok?
  end

  def type_has_method?(type : Type, method : Ast::AbstractMethod)
    overloads = env.ast_functions[method.name]? || return false
    overloads.any? do |func|
      case {func_params = func.parameters, method_params = method.parameters}
      when {Array(Parameter), Array(Ast::Parameter)}
        func_params.size == method_params.size &&
        func_params.zip(method_params).all? do |param, method_param|
          method_param_type = (
            method_param.type == Ast::Type.new(Location.zero, "Self") ? 
            type : eval(method_param.type)
          )
          param.type == method_param_type && 
          param.mode == (method_param.convention || method_param_type.mode)
        end
      when {nil, nil}
        true
      else
        false
      # end && if (ret_type = method.return_type)
      #   func.return_type == eval(ret_type)
      # else
      #   func.return_type == Type.nil
      # end
      end && func.return_type == (method.return_type.try(&->eval(Ast::Type)) || Type.nil)
    end
  end

  def eval_trait(trait : Ast::Type)
    unless trait_base = env.traits[trait.name]
      raise "TypeEnv#eval_trait: unknown trait: #{trait.name}"
    end
    Trait.new(
      trait_base, 
      eval_type_args("trait #{trait.name}", trait_base.type_params, trait.type_args)
    )
  end

  def type_check(ast : Ast::Expr) : Hir
    case ast
    when Ast::Nil
      Hir::Nil.new(ast.location)
    when Ast::True
      Hir::True.new(ast.location)
    when Ast::False
      Hir::False.new(ast.location)
    when Ast::Int
      Hir::Int.new(ast.location, ast.value)
    when Ast::Float
      Hir::Float.new(ast.location, ast.value)
    when Ast::String
      Hir::String.new(ast.location, ast.value)
    when Ast::Identifier
      identifier(ast.location, ast.name)
    when Ast::Array
      elements = ast.elements.map { |e| type_check(e) }
      element_type = elements.first.type
      Hir::Array.new(ast.location, element_type, elements)
    when Ast::NegNode
      function_call(ast.location, "-", [type_check(ast.value)])
    when Ast::NotNode
      function_call(ast.location, "not", [type_check(ast.value)])
    when Ast::Binop
      case ast.operator
      when Operator::Assign
        expr = type_check(ast.right)
        assign(ast.left.value, expr)
      else
        left = type_check(ast.left)
        right = type_check(ast.right)
        function_call(ast.location, ast.operator.to_s, [left, right])
      end
    when Ast::Call
      function_call(ast.location, ast.method, ast.args.try &.map(&->type_check(Ast::Expr)) || [] of Hir)
    when Ast::StaticCall
      raise "static call should be implemented probably almost the same as 'Call'"
    when Ast::Var
      declare(Binding::Var, Hir::Var, ast.location, ast.name, ast.value)
    when Ast::Let
      declare(Binding::Let, Hir::Let, ast.location, ast.name, ast.value)
    else
      raise "TypeEnv#type_check: unknown AST type: #{ast.class}"
    end
  rescue type_error : TypeError
    log.error(type_error.location, type_error.message)
    Hir::Nil.new(ast.location)
  end

  def type_check(ast : Cell(Ast::Expr)) : Hir
    type_check(ast.value)
  end

  def declare(binding : Binding, dec_type : Hir.class, loc : Location, name : ::String, value : Cell(Ast::Expr)? = nil) : Hir
    if value
      expr = type_check(value.value)
      variable = Variable.new(loc, binding, name, expr.type)
      scope[name] = variable
      dec_type.new(loc, name, expr)
    else
      scope[name] = Variable.new(loc, binding, name)
      dec_type.new(loc, name)
    end
  end

  def identifier(location : Location, name : ::String) : Hir
    var = scope.get_var(location, name)
    Hir::Identifier.new(location, name, var.binding, var.type.as(Type))
  end

  def function_call(location : Location, name : ::String, args : ::Array(Hir)) : Hir
    log.warning(location, "function call not fully implemented yet")
    overloads = env.functions[name]?
    if overloads.nil?
      raise TypeError.new(location, "function #{name} not found")
    end

    matches = overloads.select do |func|
      func.parameters.size == args.size &&
      func.parameters.zip(args).all? { |param, arg| param.type == arg.type }
    end

    if matches.empty?
      raise TypeError.new(location, "function call does not match any of the overloads:\n#{overloads.join("\n")}")
    end
    if matches.size > 1
      raise TypeError.new(location, "multiple overloads of function #{name} match:\n#{matches.join("\n")}")
    end
    func = matches.first
    Hir::Call.new(location, func, args, func.return_type)
  end

  def assign(lhs : Ast::Expr, rhs : Hir) : Hir
    case lhs
    when Ast::Identifier
      var = scope.assign(lhs.location, lhs.name, rhs, self)
      Hir::Assign.new(lhs.location, var, rhs)
    when Ast::Let
      var = scope.let(lhs.location, lhs.name, rhs)
      Hir::Assign.new(lhs.location, var, rhs)
    when Ast::Call
      if (ast = lhs.receiver?) && lhs.count_args == 1
        object = type_check(ast)
        if !object.mutable?
          raise TypeError.new(lhs.location, "#{object} is not mutable")
        end
        case (t = object.type)
        when Type::Struct
          if (field = t.get_field?(lhs.method))
            unify(field.type, rhs.type)
            Hir::AssignField.new(lhs.location, object, field, rhs)
          else
            raise TypeError.new(lhs.location, "#{object} has no field #{lhs.method}")
          end
        else
          raise TypeError.new(lhs.location, "#{object} is not a struct; it's #{t}")
        end
      else
        raise TypeError.new(lhs.location, "function call #{lhs} is not a valid left-hand side of assignment")
      end
    else
      raise TypeError.new(lhs.location, "#{lhs} is not a valid left-hand side of assignment")
    end
  end

  def unify(a : Type, b : Type)
    case {a, b}
    when {Type::Var, _}
      bind_var(a, b)
    when {_, Type::Var}
      bind_var(b, a)
    when {Type::Nil, Type::Nil}, 
      {Type::Bool, Type::Bool}, 
      {Type::Int, Type::Int}, 
      {Type::Float, Type::Float},
      {Type::String, Type::String}
      # Ok: equal concrete types
    when {Type::Array, Type::Array}
      unify(a.element_type.value, b.element_type.value)
    # when {FunctionType, FunctionType}
    #   # Recursively unify param and return types
    #   a_f = a.as(FunctionType)
    #   b_f = b.as(FunctionType)
    #   if a_f.param_types.size != b_f.param_types.size
    #     raise "Function arity mismatch"
    #   end
    #   a_f.param_types.zip(b_f.param_types).each do |ap, bp|
    #     unify(ap, bp)
    #   end
    #   unify(a_f.return_type, b_f.return_type)
    else
      raise "Type mismatch: #{a} vs #{b}"
    end
    :ok
  end

  def bind_var(var : Type::Var, typ : Type)
    if binding = bindings[var]?
      unify(binding, typ)
    else
      bindings[var] = typ
    end
  end
end

class TypeScope
  getter parent : TypeScope?
  getter vars = {} of String => Variable

  # retrieve the variable from the given scope, including parent scope
  # nil if not exists
  def [](name : String)
    vars[name]? || ((p=parent) && p[name])
  end

  # retrieve the named variable, but raise error if missing, uninitialized, or consumed. 
  def get_var(loc : Location, name : String)
    var = vars[name]
    raise TypeError.new(loc, "#{name} not found in scope") unless var
    raise TypeError.new(loc, "#{name} is not yet initialized") unless var.initialized?
    raise TypeError.new(loc, "#{name} was already consumed @#{var.consumed}") if var.consumed
    var
  end

  def []=(name : String, var : Variable)
    vars[name] = var
  end

  def let(loc : Location, name : String, expr : Hir)
    if name.in? vars
      print "Warning: shadowing variable #{name}\n"
    end
    vars[name] = Variable.new(loc, Binding::Let, name, expr.type)
  end

  def assign(loc : Location, name : String, expr : Hir, ctx : TypeContext)
    if var = self[name]
      raise TypeError.new(loc, "#{name} not found in scope") unless var
      raise TypeError.new(loc, "#{name} was already consumed @#{var.consumed}") if var.consumed
      if t = var.type
        ctx.unify(t, expr.type)
      else
        var.type = expr.type
      end
      var
    else
      raise TypeError.new(loc, "#{name} not found in scope")
    end
  end
end

class Variable
  getter name : String
  getter declared : Location
  property consumed : Location?
  getter binding : Binding
  property type : Type?
  def initialize(@declared : Location, @binding : Binding, @name : String, @type : Type? = nil)
  end

  def initialized?
    !type.nil?
  end

end

class TypeUnifier
  @next_id = 0
  @parents = {} of Int32 => Int32
  @types   = {} of Int32 => Type?

  # Create a new fresh type variable
  def fresh_type_var : Type::Var
    id = @next_id
    @next_id += 1
    @parents[id] = id
    @types[id] = nil
    Type::Var.new(id)
  end

  # Find the canonical representative of a type variable
  def find(var : Type::Var) : Int32
    parent = @parents[var.id]
    if parent != var.id
      root = find(Type::Var.new(parent))
      @parents[var.id] = root # path compression
      root
    else
      var.id
    end
  end

  # Get the resolved type of a type var, if known
  def get(var : Type::Var) : Type?
    root = find(var)
    @types[root]
  end

  # Unify a type var with another type
  def unify(var : Type::Var, other : Type)
    root = find(var)

    case other
    when Type::Var
      other_root = find(other)
      if root != other_root
        @parents[root] = other_root
        @types.delete(root)
      end
    else
      @types[root] = other
    end
  end

  # Fully resolve a type: follows type vars to final concrete type
  def resolve(typ : Type) : Type
    case typ
    when Type::Var
      resolved = get(typ)
      resolved ? resolve(resolved) : typ
    else
      typ
    end
  end
end

class TypeError < Exception
  property location : Location
  def initialize(@location : Location, @message : String)
  end
  def message : String
    @message || "<TYPE ERROR>"
  end
end