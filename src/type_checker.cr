require "./type_env"
require "./logger"

# Type inference context
class TypeContext
  getter env : TypeEnv
  property type_parameters = [] of TypeParameter
  property type_args = [] of Type
  property self_type : Type?
  property scope = TypeScope.new
  getter generics = {} of String => Type::Var
  getter bindings = {} of Type::Var => Type
  getter unifier = TypeUnifier.new

  # def initialize(@env, type_params : Array(TypeParameter)? = nil)
  #   if type_params
  #     type_params.each do |type_param|
  #       generics[type_param.name] = Type::Var.new(unifier.fresh_type_var.id)
  #     end
  #   end
  # end
  def initialize(@env, @type_parameters, @type_args = [] of Type, @self_type = nil)
    (@type_args.size .. @type_parameters.size).each do |i|
      @type_args << Type.var(i)
    end
  end
  def initialize(@env, ast_type_params : Array(Ast::TypeParameter)? = nil, @type_args = [] of Type, @self_type = nil)
    log.debug_descend(Location.zero, "TypeContext.new(type parameters: #{ast_type_params}, type_args: #{@type_args}, self_type: #{@self_type})") do
      if ast_type_params
        # create type parameters first with no constraints
        ast_type_params.each do |tp|
          type_parameters << TypeParameter.new(
            tp.location, 
            tp.name, 
            tp.constraints.includes ? [] of Trait : nil, 
            tp.constraints.excludes ? [] of Trait : nil
          )
        end
        # then set constraints
        ast_type_params.zip(type_parameters).each do |ast_type_param, type_param|
          if incl = ast_type_param.constraints.includes
            req = type_param.required_traits.as(Array(Trait))
            incl.each do |trait|
              req << eval_trait(trait)
            end
          end
          if excl = ast_type_param.constraints.excludes
            req_not = type_param.excluded_traits.as(Array(Trait))
            excl.each do |trait|
              req_not << eval_trait(trait)
            end
          end
        end
        @type_args = @type_args.dup
        (@type_args.size .. @type_parameters.size).each do |i|
          # raise "the problem is here: consider replacing all type_args arrays and type_params arrays with slices."
          @type_args << Type.var(i)
        end
      end
    end
  end

  def type_params_as_args : Array(Type)
    type_parameters.size.times.map { |i| Type.var(i).as Type }.to_a
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
      rescue failure : TypeError
        if msg = failure.message
          ctx.log.error(function.body.last.location, msg)
        end
        ctx.log.error(function.body.last.location, "Expected return type: #{function.return_type}; Actual return type: #{function.body.last.type}")
      end
    end
  end

  def eval(type_node : Ast::Type) : Type
    if (env.eval_depth += 1) > 100
      raise "TypeContext#eval: maximum depth exceeded"
    end
    type_parameters.each_with_index do |type_param, i|
      if type_param.name == type_node.name
        eval_type_args(type_node, nil, "Generic type ")
        return @type_args[i]
      end
    end
    # type_args = (type_node.type_args || [] of Ast::Type).map { |t| eval(t) }
    log.debug_descend(type_node.location, "eval(#{type_node})") do
      case type_node.name
      when "Never"
        eval_type_args(type_node, nil, "Primitive type ")
        Type.never
      when "Nil" 
        eval_type_args(type_node, nil, "Primitive type ")
        Type.nil
      when "Bool" 
        eval_type_args(type_node, nil, "Primitive type ")
        Type.bool
      when "Int" 
        eval_type_args(type_node, nil, "Primitive type ")
        Type.int
      when "Float" 
        eval_type_args(type_node, nil, "Primitive type ")
        Type.float
      when "String" 
        eval_type_args(type_node, nil)
        Type.string
      when "Array"
        type_args = eval_type_args(type_node, [TypeParameter.new(Location.zero, "T")])
        Type.array(type_args[0]? || Type.never)
      when "Tuple"
        Type.tuple((type_node.type_args || [] of Ast::Type).map { |t| eval(t) })
      when "Self"
        eval_type_args(type_node, nil, "Special type Self")
        @self_type || Type.self
      # when "Function" then Type.function(*type_node.types.map { |t| eval(t) })
      else
        case base = env.user_types[type_node.name]?
        when StructBase
          Type.struct(
            base, 
            eval_type_args(type_node, base.type_params)
          )
        when EnumBase
          Type.enum(
            base, 
            eval_type_args(type_node, base.type_params)
          )
        else
          log.error(type_node.location, "eval: unknown type: #{type_node.name}")
          Type.unknown(type_node.name, type_node.type_args.try &.map { |t| eval(t).as Type } || [] of Type)
        end
      end
    end
  rescue err : TypeError
    if env.eval_depth > 100
      raise err
    end
    log.error(type_node.location, err.message || "TYPE ERROR @ {{__line}}")
    Type.never
  ensure
    env.eval_depth -= 1
  end

  def eval_type_args(type_node : Ast::Type, type_params : Array(TypeParameter)?, error_prefix = "") : Array(Type)
    log.debug_descend(type_node.location, "#eval_type_args(#{type_node}, #{type_params}, #{error_prefix})") do
      if (env.eval_depth += 1) > 100
        raise "TypeContext#eval_type_args: maximum depth exceeded"
      end
      case {type_params, t_args = type_node.type_args}
      in {nil, nil}
        [] of Type
      in {nil, Array(Ast::Type)}
        log.error(type_node.location, "#{error_prefix}#{type_node.name} does not expect type arguments, but #{t_args.size} were provided.") unless t_args.size == 0
        [] of Type
      in {Array(TypeParameter), nil}
        log.error(type_node.location, "#{error_prefix}#{type_node.name} expects #{type_params.size} type arguments, but none were provided.") unless type_params.size == 0
        [] of Type
      in {Array(TypeParameter), Array(Ast::Type)}
        if t_args.size != type_params.size
          log.error(type_node.location, "#{error_prefix}#{type_node.name} expects #{type_params.size} type arguments, but #{t_args.size} were provided.")
        end
        type_params.zip(t_args).map do |type_param, type_arg|
          eval(type_arg, type_param)
        end
      end
    end
  ensure
    env.eval_depth -= 1
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

  def type_satisfies_constraints(loc : Location, type : Type, type_param : TypeParameter)
    log.debug_descend(loc, "TypeEnv#type_satisfies_constraints: #{type} satisfies #{type_param}") do
      if incl = type_param.required_traits
        incl.each do |trait|
          log.debug_descend
          unless type_implements_trait?(type, trait)
            log.error(loc, "TypeEnv#type_satisfies_constraints: type #{type} does not satisfy trait #{trait}")
          end
        end
      end
      if excl = type_param.excluded_traits
        excl.each do |trait|
          if type_implements_trait?(type, trait)
            log.error(loc, "TypeEnv#type_satisfies_constraints: type #{type} satisfies excluded trait #{trait}")
          end
        end
      end
    end
    nil
  end

  def eval(type_node : Ast::Type, constraints : TypeParameter)
    if (env.eval_depth += 1) > 100
      raise "TypeChecker#eval: maximum depth exceeded"
    end
    type = eval(type_node)
    if type == Type.self
      log.debug(type_node.location, "Skipping type validation for `Self`")
    elsif env.ready_to_validate_types
      log.debug_descend(type_node.location, "validating #{type} with constraints: #{constraints}") do
        type_satisfies_constraints(type_node.location, type, constraints)
      end
    else
      log.debug(type_node.location, "Skipping type validation for #{type} with constraints: #{constraints}")
    end
    type
  ensure
    env.eval_depth -= 1
  end

  # Check if each required method of a trait is implemented for the given type
  # Caches results.
  def type_implements_trait?(type : Type, trait : Trait)
    trait = trait.substitute_Self_with(type)
    log.debug_descend(Location.zero, "#type_implements_trait?: #{type} implements #{trait}") do
      if impl = env.implementations[{type, trait}]?
        log.debug(Location.zero, "#{type} implements #{trait}?: #{impl} (cached)")
        impl.ok?
      else
        env.implementations[{type, trait}] = Implements::Calculating
        _type_implements_trait?(type, trait).ok?
      end
    end
  end

  # Check if each required method of a trait is implemented for the given type
  def _type_implements_trait?(type : Type, trait : Trait) : Implements
    log.debug_descend(Location.zero, "#_type_implements_trait? #{type}, #{trait}") do
      env.implementations[{type, trait}] = 
        if trait.base.methods.all? { |method| method.body || (
          errors = type_has_method?(type, method, trait.type_args)
          if errors.nil?
            true
          else
            errors.each do |err|
              log.error(err.location, err.message)
            end
            false
          end
        )}
          Implements::True
        else
          Implements::False
        end
    end
  end

  # When asking if a type implements a given abstract method, we must provide
  # the list of type_args of the trait owning the method so that types can be substituted
  def type_has_method?(type : Type, method : Ast::AbstractMethod, type_args : Array(Type)) : Array(TypeError)?
    log.debug_descend(Location.zero, "#type_has_method?(#{type}, #{method})") do
      overloads = env.functions[method.name]?
      unless overloads
        return [TypeError.new(Location.zero, "#{method.name} does not exist.")]
      end
      errors = [] of TypeError
      overloads.each do |func|
        case {func_params = func.parameters, method_params = method.parameters}
        when {Array(Parameter), Array(Ast::Parameter)}
          if func_params.size != method_params.size
            errors << TypeError.new(func.location, "#{method.name} requires #{method_params.size} parameters, but #{func_params.size} were provided.")
            next
          end
          method_context = TypeContext.new(env, method.type_params, type_args, self_type: type)
          params_match = log.debug_descend(method.location, "context switch to method to eval parameters of method #{method} (context=#{method_context})") do
            # WAIT!  You know what we gotta do here?  We have to actually *substitute* some type params."
            # So if we have `Equatable[T]` requiring `def ==(other T)`, 
            # then `Equatable[Int]` should require `def ==(other Int)`! 
            # I guess what we actually have to do is give TypeContext a *map* of String => Type, where the type 
            # isn't necessarily a type var, in fact, this is a case where a type var should be substituted for
            # a real type
            # ... OR maybe I need type parameters AND a type-substitution map...
            func_params.zip(method_params).all? do |param, method_param|
              # method_param_type = if method_param.type == Ast::Type.new(Location.zero, "Self")
              #   type
              # else
              #   t = method_context.eval(method_param.type)
              #   if t.is_a? Type::Var
              #     p! func.type_params[t.id]
              #   else
              #     t
              #   end
              # end
              method_param_type = method_context.eval(method_param.type)
              if param.type != method_param_type
                errors << TypeError.new(func.location, "#{method.name} requires parameter #{method_param.name} to be of type #{method_param_type}, but #{param.type} was provided.")
                false
              elsif param.mode != (method_param.convention || method_param_type.mode)
                errors << TypeError.new(func.location, "#{method.name} requires parameter #{method_param.name} to be of mode #{method_param.convention || method_param_type.mode}, but #{param.mode} was provided.")
                false
              else
                true
              end
            end
          end
          next unless params_match
        when {nil, nil}
          # so far so good
        when {Array(Parameter), nil}
          errors << TypeError.new(func.location, "#{method.name} requires no parameters, but #{func_params.size} were provided.")
          next
        when {nil, Array(Ast::Parameter)}
          errors << TypeError.new(func.location, "#{method.name} requires #{method_params.size} parameters, but none were provided.")
          next
        else
          p! func_params
          p! method_params
          raise "impossible"
        end
        if func.return_type == (method.return_type.try(&->eval(Ast::Type)) || Type.nil)
          return nil
        else
          errors << TypeError.new(func.location, "#{method.name} requires return type #{method.return_type}, but #{func.return_type} was provided.")
        end
      end
      errors
    end
  end

  def eval_trait(trait : Ast::Type)
    log.debug(trait.location, "eval_trait #{trait}")
    unless trait_base = env.traits[trait.name]?
      raise TypeError.new(trait.location, "unknown trait: #{trait.name}")
    end 
    p! trait.name
    p! trait.type_args
    p! eval_type_args(trait, trait_base.type_params)
    if trait.name == "Equatable" && (trait.type_args.try &.size || 0) > 1
      raise TypeError.new(trait.location, "Equatable can only have one type arg")
    end
    Trait.new(
      trait_base, 
      begin
        eval_type_args(trait, trait_base.type_params)
      rescue err : TypeError
        log.error(trait.location, err.message || "<type arg error")
        [] of Type
      end
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

  def to_s(io : IO)
    io << "TypeContext(" 
    io << "type_parameters: #{type_parameters.join(", ")}, " if type_parameters.any?
    io << "type_args: #{type_args.join(", ")}, " if type_args.any?
    io << "self_type: #{self_type}, " if self_type
    io << "scope: #{scope}"
    io << ")"
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