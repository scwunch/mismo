require "./tokens"
require "./ast_nodes"
require "../utils/logger" 


# Defines the types of tokens that can stop an expression parser
enum StopAt
  Normal             # Default: stops at block closers, major separators like Colon, EOF
  # BooleanOperator    # Stops if the next token is a boolean comparison operator
  # ColonOrAnd         # Stops at Colon or an 'and' keyword/operator
  UcsBranch          # Stops at an indented line iff the last token was a boolean operator or an 'and'
  UcsEnd             # Stops at an 'and' token or colon
  Comma              # Stops at Comma (e.g., for arguments in a list)
  Newline            # Stops at a Newline token
  ExpressionEnd      # Stops more aggressively, e.g. doesn't consume trailing operators
end

enum Expecting
  Term
  Operator
end

# methods for peek, next_token, consume, and error-handling
module TokenNavigation
  @index = 0_u32
  @tokens : Array(Token)

  # Peeks at the current token without consuming
  def peek : Token
    @tokens[@index]? || Token.eof(Location.zero)
  end

  def peek2 : Token
    @tokens[@index + 1]? || peek
  end

  # Consumes the current token and returns it. Advances the index.
  # Returns the last token if already at EOF to prevent errors on multiple calls.
  def next_token : Token
    case tok = @tokens[@index]?
    when Nil
      return Token.eof(Location.zero)
    when Token::Newline
      @current_line_indent = tok.data
    end
    @index += 1
    tok
  end

  # Checks if the parser has reached the end of the token stream.
  def eof? : Bool
    peek.is_a?(Token::EOF)
  end

  # Reads an optional convention keyword (let, mut, move, copy, ref)
  def read_convention? : Convention # Convention = Mode | Nil
    tok = peek
    if tok.is_a?(Token::KeyWord)
      case tok.data
      when KeyWord::Let   then next_token; Mode::Let
      when KeyWord::Mut   then next_token; Mode::Mut
      when KeyWord::Move  then next_token; Mode::Move
      when KeyWord::Box   then next_token; Mode::Box
      when KeyWord::Ref   then next_token; Mode::Ref
      else 
        nil  # Not a convention keyword
      end
    else
      nil # No convention keyword found
    end
  end

  def read_binding? : Binding?
    tok = peek
    if tok.is_a?(Token::KeyWord)
      case tok.data
      when KeyWord::Var   then next_token; Binding::Var
      when KeyWord::Let   then next_token; Binding::Let
      when KeyWord::Mut   then next_token; Binding::Mut
      when KeyWord::Box   then next_token; Binding::Box
      when KeyWord::Ref   then next_token; Binding::Ref
      else 
        nil  # Not a binding keyword
      end
    else
      nil # No binding keyword found
    end
  end

  def consume?(&block : Token -> Bool) : Token?
    if yield peek
      next_token
    else
      nil
    end
  end

  # Consumes the current token if it's of the expected kind.
  # If not, reports an error and returns nil.
  def consume?(expected_kind : Token.class) : Token?
    if peek.class == expected_kind
      next_token
    end
  end

  # Overload for specific token data
  def consume?(expected_data : KeyWord | Operator | String) : Token?
    if peek.data == expected_data
      next_token
    end
  end

  def consume!(expected)
    consume?(expected) || raise report_error(peek.location, "Expected #{expected}, got #{peek}")
  end

  # Consumes tokens until one of the `target_kinds` is encountered or EOF.
  # Returns the token that matched or the EOF token.
  def consume_until(*target_kinds : Token.class) : Token
    skipped_tokens = [] of Token
    while !eof?
      current = peek
      return current if target_kinds.any? { |kind| current.class == kind }
      skipped_tokens << next_token
    end
    @log.debug(peek.location, "Consumed until EOF. Skipped: #{skipped_tokens.join(", ")}") unless skipped_tokens.empty?
    peek # EOF
  end

  def consume_until(kind : Token.class)
    while true
      case peek
      when kind
        return peek
      when Token::EOF
        return
      else
        next_token
      end
    end
  end

  def consume_until(kind1 : Token.class, kind2 : Token.class)
    while true
      case peek
      when kind1, kind2
        return peek
      when Token::EOF
        return
      else
        next_token
      end
    end
  end

  # --- Identifier Consumption ---
  # Consumes the next token as if it is an identifier.
  # Works for variable names, type names, keywords, and operators;
  # raises a ParseError otherwise
  def consume_identifier(context_message : String = "identifier") : String
    # case token = ignore_newline ? next_token(:skip_newline) : next_token
    case token = next_token
    when Token::Variable, Token::Type # Both can be identifiers in different contexts
      token.data
    when Token::KeyWord, Token::Operator
      token.data.to_s
    when Token::Not
      "not"
    else
      raise report_error(token.location, "Expected #{context_message}, got #{token}")
    end
  end

  def consume_type_name! : String
    case token = next_token
    when Token::Type
      token.data
    when Token::Variable # Check if it's a mis-cased type name
      report_error(token.location, "Type names must be capitalized; got '#{token.data}' (a variable name pattern).")
      # We still consume it and let semantic analysis catch it later
      token.data
    else
      raise report_error(token.location, "Expected type name, got #{token}")
    end
  end

  def consume_type_name?(ignore_newline = false) : String?
    if (tok = peek).is_a?(Token::Type)
      next_token.as(Token::Type).data
    end
  end

  # Consumes a newline only if it begins a new block and returns that indent.
  # Otherwise returns nil.
  def consume_indent?(block_indent : UInt32) : UInt32?
    log.debug(peek.location, "consume_indent? (block_indent: #{block_indent}) # peek: #{peek}")
    if (tok = peek).is_a?(Token::Newline)
      if tok.data > block_indent
        next_token
        tok.data
      end
    end
  end

  # Consumes a newline only if it's not the end of a block
  # Returns true if end of block found
  # End of block is determined to be a newline with indent ≤ block_indent
  # EXCEPT if the token after newline is a closing bracket or `else`
  def skip_newline_unless_block_end?(block_indent : UInt32, raise_on_outdent : String? = nil) : Bool
    if (tok = peek).is_a?(Token::Newline)
      if tok.data <= block_indent
        unless peek2.class.in?(Token::RBracket, Token::RBrace, Token::RParen, Token::Else)
          if raise_on_outdent
            raise report_error(tok.location, raise_on_outdent)
          else
            log.debug(tok.location, "outdent detected; not skipping newline")
            return true
          end
        end
        log.debug(tok.location, "outdent detected, but followed by #{peek2}")
      end
      log.debug(tok.location, "skipping newline")
      next_token  # consume newline
    end
    log.debug(tok.location, "no newline to skip")
    false
  end

  # Consumes a comma or newline acting as a list separator
  # Returns the consumed token or nil if end of list
  # Trailing comma and/or newline are consumed but not returned.
  def consume_list_separator?(list_indent) : Token::Comma | Token::Newline | Nil
    case tok = peek
    when Token::Newline
      skip_newline_unless_block_end?(list_indent, "Unexpected outdent in list.")
      case peek
      when Token::Comma
        consume_list_separator?(list_indent)
      when Token::RBracket, Token::RParen
        nil  # end of list
      else
        tok  # newline as separator
      end
    when Token::Comma
      next_token
      if (newline = peek).is_a?(Token::Newline)
        if peek2.class.in?(Token::RBracket, Token::RParen)
          next_token  # consume newline after trailing comma
          return nil  # trailing comma, end of list
        elsif newline.data <= list_indent
          raise report_error(newline.location, "Unexpected outdent in list after comma.")
        else
          next_token  # consume newline after comma
        end
      end
      tok  # comma separator
    else
      nil  # no separator
    end
  end

  # --- Error Reporting & Recovery ---
  def report_error(location : Location, message : String) : Parser::ParseError
    @log.error(location, message)
    Parser::ParseError.new(message, location)
  end

  def recover_to_next_declaration
    # Simple recovery: skip tokens until we find a keyword that typically starts a new declaration
    @log.debug(peek.location, "Attempting recovery...")
    until eof?
      tok = peek
      # Check for top-level keywords
      case tok.data
      when "struct", "enum", "trait", "extend", "def", "import"
        @log.debug(tok.location, "Recovery found keyword #{tok.data}, resuming parse.")
        return
      end
      # if tok.is_a?(Token::KeyWord)
      #   case tok.data
      #   when KeyWord::Struct, KeyWord::Enum, KeyWord::Trait, 
      #        KeyWord::Extend, KeyWord::Def, KeyWord::Import
      #     @log.debug(tok.location, "Recovery found keyword #{tok.data}, resuming parse.")
      #     return
      #   end
      # end
      next_token # Consume and discard the token
    end
    @log.debug(peek.location, "Recovery reached EOF or skipped too many tokens.")
  end

  def recover_to_next_def_or_sub_item
    @log.debug(peek.location, "Attempting recovery to next sub-item...")
    until eof?
      if peek.is_a?(Token::KeyWord)
        case peek.data
        when KeyWord::Struct, KeyWord::Enum, KeyWord::Trait, 
             KeyWord::Extend, KeyWord::Import,
             KeyWord::Def, KeyWord::Static, KeyWord::Constructor
          @log.debug(peek.location, "Recovery found #{peek.data}, resuming parse.")
          return
        end
      end
      next_token
    end
    @log.debug(peek.location, "Recovery in top-level block reached EOF.")
  end
end

enum TopLevelKey
  Import
  Extern
  Struct
  Enum
  Function
  Extend
  Trait
  Field
  Constructor
  Static
  Def
  Const

  def self.parse?(str)
    case str
    when "import" then Import
    when "extern" then Extern
    when "struct" then Struct
    when "enum" then Enum
    when "function" then Function
    when "extend" then Extend
    when "trait" then Trait
    when "field" then Field
    when "constructor" then Constructor
    when "static" then Static
    when "def" then Def
    when "const" then Const
    else
      nil
    end
  end
end

module TopLevelItemParser
  def parse_top_level_item : Ast::TopLevelItem?
    loc = peek.location
    @log.debug_descend(loc, "Parsing top-level item, current token: #{peek}") do
      case next_token.data
      when "import"
        parse_import(loc)
      when "struct"
        parse_struct(loc)
      when "enum"
        parse_enum(loc)
      when "trait"
        parse_trait(loc)
      when "extend"
        parse_extend(loc)
      when "def"
        parse_def_block(loc)
        nil  # so we don't add some declarations twice
      when "extern"
        consume!("def")
        parse_def_block(loc, external: true)
        nil
      # when "const"
      #   parse_const(loc)
      else
        raise report_error(peek.location, "Expected top-level declaration (struct, function, enum, etc.); got #{peek}")
      end
    end
  end

  def parse_def_block(
    def_loc : Location, 
    inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty, 
    receiver : Ast::Parameter? = nil, 
    trait_method_array : Array(Ast::AbstractMethod)? = nil,
    external = false
  )
    inherited_type_params = parse_type_parameters? if inherited_type_params.empty?
    tree_branch(def_loc) do |is_branch|
      name_loc = is_branch ? peek.location : def_loc
      function_name = consume_identifier("function name")
      tree_branch(name_loc) do |is_branch|
        type_params_loc = is_branch ? peek.location : name_loc
        type_params = parse_type_parameters?(inherited_type_params)
        tree_branch(type_params_loc) do |is_branch|
          sig_loc = is_branch ? peek.location : type_params_loc
          sig = parse_signature(sig_loc, type_params, receiver)          
          if trait_method_array
            block = peek.is_a?(Token::Colon) ? parse_colon_and_block(sig_loc.indent) : nil
            method = Ast::AbstractMethod.new(sig_loc, function_name, sig, block)
            log.info(sig_loc, "parsed #{method}")
            trait_method_array << method
            method
          elsif external
            sig = Ast::ExternalFunction.new(sig_loc, function_name, sig)
            log.info(sig_loc, "parsed #{sig}")
            @declarations << sig
            sig
          else
            block = parse_colon_and_block(sig_loc.indent)
            func = Ast::Function.new(sig_loc, function_name, sig, block)
            log.info(sig_loc, "parsed #{func}")
            @declarations << func
            func
          end
        end
      end
    end
  end

  def parse_signature(sig_loc : Location, 
                      inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty,
                      receiver : Ast::Parameter? = nil, 
                     ) : Ast::Signature?
    @log.debug_descend(sig_loc, "Parsing signature...") do
      declaration_indent = @current_line_indent
      type_params = parse_type_parameters?(inherited_type_params)
      skip_newline_unless_block_end?(declaration_indent)
      params : Array(Ast::Parameter) = if receiver 
        parse_parameters([receiver])
      else
        parse_parameters?
      end
      convention = nil
      return_type = 
        if consume?(Operator::RArrow)
          convention = read_convention?
          parse_type_expression
        elsif convention = read_convention?
          parse_type_expression
        elsif peek.is_a?(Token::Type)
          parse_type_expression
        else
          nil
        end
      if return_type && return_type.binding
        report_error(return_type.location, "Return type cannot have a binding, only a passing convention.")
      end
      Ast::Signature.new(sig_loc, type_params, params, return_type, convention)
    end
  end

  # Parses `[T, U: Constraint]` or `[A, B: Constraint1 & Constraint2]`
  def parse_type_parameters?(inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty) : Slice(Ast::TypeParameter) 
    # type_params = inherited_type_params.try &.dup
    return inherited_type_params unless peek.is_a?(Token::LBracket)
    params_indent = @current_line_indent
    # type_params ||= [] of Ast::TypeParameter
    lbracket_loc = next_token.location  # consume '['
    @log.debug(peek.location, "about to possibly skip a newline...")
    skip_newline_unless_block_end?(params_indent, "Unexpected outdent in type parameters.")
    if consume?(Token::RBracket)
      return inherited_type_params
    end
    type_params = inherited_type_params
    
    @log.debug_descend(lbracket_loc, "Parsing type parameters...") do
      until eof?
        # parse type parameter
        @log.debug(peek.location, "parsing type parameter")
        param_loc = peek.location
        name = consume_type_name!
        constraints = parse_constraints
        # type_params << Ast::TypeParameter.new(param_loc, name, constraints)
        type_params = type_params.push(Ast::TypeParameter.new(param_loc, name, constraints))
        consume_list_separator?(params_indent) || break
      end
      consume!(Token::RBracket)
    end
    type_params
  end

  # Parses `is Trait & Trait[Int]`
  def parse_traits?
    # consume colon or `is`
    return nil unless consume?(Token::Colon) || consume?(Operator::Is)
    traits = [] of Ast::Type
    @log.debug_descend(peek.location, "Parsing traits...") do
      until eof?
        skip_newline_unless_block_end?(0, "Unexpected outdent in traits parsing.")
        traits << parse_type_expression
        break if skip_newline_unless_block_end?(0)
        break unless consume?(Operator::And)
      end
      skip_newline_unless_block_end?(0)
      traits
    end
  end

  # Parses `: Trait & Trait[Int]`
  def parse_constraints : Ast::Constraints
    unless consume?(Token::Colon) || consume?(Operator::Is) || peek.data == Operator::Not || peek.is_a?(Token::Type)
      return Ast::Constraints.new
    end
    includes = Slice(Ast::Type).empty
    excludes = Slice(Ast::Type).empty
    type_line_indent = @current_line_indent
    @log.debug_descend(peek.location, "Parsing constraints...") do
      skip_newline_unless_block_end?(type_line_indent, "Unexpected outdent in constraints.")
      # parse first constraint
      if consume?(Operator::Not)
        skip_newline_unless_block_end?(type_line_indent, "Unexpected outdent in constraints.")
        push!(excludes, parse_type_expression)
      else
        push!(includes, parse_type_expression)
      end
      # parse remaining constraints
      until eof?
        skip_newline_unless_block_end?(type_line_indent, "Unexpected outdent in constraints.")
        if consume?(Operator::And)
          skip_newline_unless_block_end?(type_line_indent, "Unexpected outdent in constraints.")
          push!(includes, parse_type_expression)
        elsif consume?(Operator::Not)
          skip_newline_unless_block_end?(type_line_indent, "Unexpected outdent in constraints.")
          push!(excludes, parse_type_expression)
        else
          break
        end
      end
    end
    Ast::Constraints.new(includes, excludes)
  end

  def parse_parameters(params : Array(Ast::Parameter))
    return params unless consume?(Token::LParen)
    fn_indent = @current_line_indent
    skip_newline_unless_block_end?(fn_indent, "Unexpected outdent in empty parameters.")
    return params if consume?(Token::RParen)

    @log.debug_descend(peek.location, "Parsing parameters...") do
      until eof?
        params << parse_parameter
        consume_list_separator?(fn_indent) || break
      end
      consume!(Token::RParen)
      params
    end
  end
  
  # Parses (p1: Type, p2: Type)
  def parse_parameters? : Array(Ast::Parameter) 
    return [] of Ast::Parameter unless peek.is_a?(Token::LParen)
    parse_parameters([] of Ast::Parameter)
  end

  def parse_parameter : Ast::Parameter
    loc = peek.location
    convention = read_convention?
    name = consume_identifier("parameter name")
    # optional colon
    consume?(Token::Colon)
    type = parse_type_expression
    if convention && type.binding && convention.to_binding != type.binding
      report_error(loc, "Parameter convention #{convention} does not match type binding #{type.binding}")
    end
    Ast::Parameter.new(loc, convention, name, type)
  end

  # Parses a type expression, e.g., MyType, MyGenericType[Arg1, Arg2]
  def parse_type_expression : Ast::Type
    loc = peek.location
    binding = read_binding?
    name = consume_type_name!
    @log.debug_descend(loc, "Parsing type expression: #{name}") do
      type_args = parse_type_args?
      Ast::Type.new(loc, name, type_args, binding)
    end
  end

  # Parses type arguments, e.g., [Arg1, Arg2]
  def parse_type_args? : Slice(Ast::Type) 
    consume?(Token::LBracket) || return Slice(Ast::Type).empty
    type_line_indent = @current_line_indent
    skip_newline_unless_block_end?(type_line_indent, "Expected type argument; got end of block.")
    type_args = Slice(Ast::Type).empty
    return type_args if consume?(Token::RBracket)
    
    @log.debug_descend(peek.location, "Parsing type arguments...") do
      until eof?
        push!(type_args, parse_type_expression)
        consume_list_separator?(type_line_indent) || break
      end
      consume!(Token::RBracket)
      type_args
    end
  end

  # Parses `: BLOCK_OF_STATEMENTS`
  def parse_colon_and_block(block_base_indent : UInt32 = @current_line_indent) : Ast::Expr
    @log.debug_descend(peek.location, "Parsing colon and block (base indent: #{block_base_indent})...") do
      consume!(Token::Colon)
      loc = peek.location
      # Statements are parsed relative to the block_base_indent.
      # A statement belongs to the block if it's indented further than block_base_indent.
      statements = [] of Ast::Expr
      until eof?
        @log.debug(peek.location, "Parsing statement starting with #{peek} (base indent: #{block_base_indent})")
        case tok = peek
        when Token::Newline
          break if tok.data <= block_base_indent
          next_token
        when Token::EOF, Token::RBrace, Token::RParen, Token::RBracket, Token::Else
          break
        else
          break if tok.location.column <= block_base_indent
          statements << parse_expression(peek.location.indent)
        end
      end
      case statements.size
      when 0
        raise "empty block"
      when 1
        statements[0]
      else
        Ast::Block.new(loc, statements)
      end
    end
  end
  
  # Parses a sequence of statements until indent decreases or block ends
  # def parse_statements(current_block_indent : UInt32, delimiter : StopAt = StopAt::Normal) : Array(Ast::Expr)
    # @log.debug_descend(peek.location, "parse_statements (indent: #{current_block_indent}, stop: #{delimiter}) - NOT FULLY IMPLEMENTED") do
    #   statements = [] of Ast::Expr
    #   until eof?
    #     @log.debug(peek.location, "Parsing statement starting with #{peek}")
    #     case tok = peek
    #     when Token::Newline
    #       break if tok.data <= current_block_indent
    #       next_token
    #     when Token::EOF, Token::RBrace, Token::RParen, Token::RBracket
    #       break
    #     else
    #       break if tok.location.column <= current_block_indent
    #       statements << parse_expression(peek.location.indent)
    #     end
    #   end
    #   statements
    # end
  # end

  def parse_type_header : {Convention, String, Slice(Ast::TypeParameter) , Array(Ast::Type)? }
    convention = read_convention?
    name = consume_identifier
    type_params = parse_type_parameters?
    traits = parse_traits?
    {convention, name, type_params, traits}
  end
    

  # parses a struct, including traits, fields, methods, and any other sub-items
  # ```
  # struct Point is Stringable
  #   field x Int
  #   field y Int
  # ```
  def parse_struct(decl_loc : Location) : Ast::Struct?
    @log.debug_descend(decl_loc, "Parsing struct declaration...") do
      convention, name, type_params, traits = parse_type_header
      struct_dec = Ast::Struct.new(decl_loc, convention, name, type_params, traits)
      receiver = Ast::Parameter.new(decl_loc, "self", Ast::Type.new(decl_loc, name, Ast.to_type_args(type_params)))
      until eof?
        break if skip_newline_unless_block_end?(decl_loc.indent)
        if (key = consume?(Token::KeyWord)) && key.data == KeyWord::Var
          struct_dec.fields << parse_field(key.location)
        elsif str_tok = consume?(Token::String)
          @log.info(str_tok.location, "Skipping docstring:\n#{str_tok.data}")
        else
          key_tok = consume!(Token::Variable)
          case key_tok.data
          when "field"
            struct_dec.fields << parse_field(key_tok.location)
          when "def"
            parse_method(key_tok.location, receiver, type_params)
          when "constructor"
            @declarations << parse_constructor(key_tok.location, receiver.type, type_params)
          when "static"
            @declarations << parse_static(key_tok.location, name, type_params)
          else
            report_error(peek.location, "Unexpected token in struct declaration")
            break
          end
        end
      end
      @declarations << struct_dec
      struct_dec
    end
  end

  def parse_field(loc : Location) : Ast::Field
    @log.debug_descend(loc, "Parsing field...") do
      name = consume_identifier
      # optional colon
      consume?(Token::Colon)
      type = parse_type_expression
      init = consume?(Operator::Assign) && parse_expression(loc.indent)
      Ast::Field.new(loc, name, type, init)
    end
  end

  # Parses a def method in a type declaration block.
  # Examples:
  # - `def method_name(param String) -> return_type: <function body>`
  # - `def mut mutable_method(param Int): <function body>`
  # - `def foo[T](a T) -> let T: <function body>`
  # - `def +(other Self) -> Self: <function body>`    -- overloads + operator
  # - `def field_name -> FieldType: <function body>`  -- acts as a getter
  # - `def String String: <function body>`            -- overloads String constructor
  def parse_method(loc : Location, receiver : Ast::Parameter, inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty)
    @log.debug_descend(loc, "Parsing method with receiver: #{receiver}, inherited type params: #{inherited_type_params}") do
      convention = read_convention?
      receiver.convention = convention if convention
      # name = consume_identifier
      parse_def_block(loc, inherited_type_params, receiver)
    end
  end

  # def parse_abstract_method(loc : Location, receiver : Ast::Parameter, inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty) : Ast::AbstractMethod
  #   @log.debug_descend(loc, "Parsing abstract method with receiver: #{receiver}, inherited type params: #{inherited_type_params}") do
  #     convention = read_convention?
  #     receiver.convention = convention if convention
  #     name = consume_identifier
  #     signature = parse_signature(peek.location, inherited_type_params, receiver)
  #     if peek.is_a?(Token::Colon)
  #       block = parse_colon_and_block(loc.indent)
  #       consume?(Token::Newline) || report_error(peek.location, "Expected newline after method body")
  #     end
  #     Ast::AbstractMethod.new(loc, name, signature, block)
  #   end
  # end

  def parse_abstract_method(loc : Location, receiver : Ast::Parameter, inherited_type_params : Slice(Ast::TypeParameter) , trait_methods : Array(Ast::AbstractMethod))
    @log.debug_descend(loc, "Parsing abstract method with receiver: #{receiver}, inherited type params: #{inherited_type_params}") do
      convention = read_convention?
      receiver.convention = convention if convention
      parse_def_block(loc, inherited_type_params, receiver, trait_methods)
    end 
  end

  def parse_constructor(loc : Location, return_type : Ast::Type, type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty)
    @log.debug_descend(loc, "Parsing constructor...") do
      signature = parse_signature(loc, type_params)
      if t = signature.return_type
        report_error(t.location, "Constructor cannot have a return type")
      end
      signature.return_type = return_type
      block = parse_colon_and_block(loc.indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after constructor body")
      Ast::Function.new(loc, return_type.name, signature, block)
    end
  end

  def parse_static(loc : Location, type_name : String, inherited_type_params : Slice(Ast::TypeParameter) = Slice(Ast::TypeParameter).empty)
    @log.debug_descend(loc, "Parsing static method...") do
      name = consume_identifier
      signature = parse_signature(peek.location, inherited_type_params)
      block = parse_colon_and_block(loc.indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after static method body")
      Ast::Function.new(loc, "#{type_name}.#{name}", signature, block)
    end
  end

  def parse_trait(decl_loc : Location) : Ast::Trait?
    @log.debug_descend(decl_loc, "Parsing trait declaration...") do
      convention, name, type_params, traits = parse_type_header
      # add implicit `Self` type parameter
      type_params = Slice(Ast::TypeParameter).new(type_params.size + 1) do |i|
        if i == 0
          Ast::TypeParameter.new(decl_loc, "Self")
        else
          Ast::TypeParameter.new(type_params[i - 1].location, type_params[i - 1].name)
        end
      end
      trait_dec = Ast::Trait.new(decl_loc, convention, name, type_params, traits)
      receiver = Ast::Parameter.new(decl_loc, "self", Ast::Type.new(decl_loc, "Self"))
      until eof?
        break if skip_newline_unless_block_end?(decl_loc.indent)
        if str_tok = consume?(Token::String)
          @log.info(str_tok.location, "Skipping docstring:\n#{str_tok.data}")
          next
        end
        key_tok = consume!(Token::Variable)
        case key_tok.data
        when "field"
          report_error(key_tok.location, "Fields not yet supported by traits.")
          parse_field(key_tok.location)
        when "def"
          parse_abstract_method(key_tok.location, receiver, type_params, trait_dec.methods)
        when "constructor"
          report_error(key_tok.location, "Constructors not yet supported by traits.")
          parse_constructor(key_tok.location, receiver.type, type_params)
        when "static"
          @declarations << parse_static(key_tok.location, name, type_params)
        else
          report_error(peek.location, "Unexpected token in trait declaration")
          break
        end
      end
      @declarations << trait_dec
      trait_dec
    end
  end

  def parse_enum(decl_loc : Location) : Ast::Enum?
    @log.debug_descend(decl_loc, "Parsing enum declaration...") do
      convention, name, type_params, traits = parse_type_header
      enum_dec = Ast::Enum.new(decl_loc, convention, name, type_params, traits)
      receiver = Ast::Parameter.new(decl_loc, "self", Ast::Type.new(decl_loc, name, Ast.to_type_args(type_params)))
      until eof?
        break if skip_newline_unless_block_end?(decl_loc.indent)
        if variant_token = consume?(Token::Type)
          enum_dec.variants << parse_variant(variant_token.location, variant_token.data.as(String))
          next
        elsif str_tok = consume?(Token::String)
          @log.info(str_tok.location, "Skipping docstring:\n#{str_tok.data}")
        end
        key_tok = consume!(Token::Variable)
        case key_tok.data
        when "field"
          # enum_dec.fields << parse_field(key_tok.location)
          report_error(key_tok.location, "Enums cannot have fields directly, fields should be in variants.")
          parse_field(key_tok.location)
        when "def"
          parse_method(key_tok.location, receiver, type_params)
        when "constructor"
          @declarations << parse_constructor(key_tok.location, receiver.type, type_params)
        when "static"
          @declarations << parse_static(key_tok.location, name, type_params)
        else
          report_error(peek.location, "Unexpected token in enum declaration")
          break
        end
      end
      @declarations << enum_dec
      enum_dec
    end
  end

  def parse_variant(loc : Location, name : String) : Ast::Variant
    @log.debug_descend(loc, "Parsing variant declaration...") do
      unless consume?(Token::LParen)
        return Ast::Variant.new(loc, name)
      end
      variant_line_indent = @current_line_indent
      skip_newline_unless_block_end?(variant_line_indent, "Expected variant field; got end of block.")
      if consume?(Token::RParen)
        @log.warning(loc, "Variant #{name} has no fields.")
        return Ast::Variant.new(loc, name, [] of {String, Ast::Type})
      end
      fields = [] of {String, Ast::Type}
      until eof?
        case peek
        when Token::Type
          field_type = parse_type_expression
          fields << {fields.size.to_s, field_type}
        else
          field_name = consume_identifier
          consume?(Token::Colon)
          field_type = parse_type_expression
          fields << {field_name, field_type}
        end
        consume_list_separator?(variant_line_indent) || break
      end
      consume!(Token::RParen)
      Ast::Variant.new(loc, name, fields)
    end
  end

  def parse_import(decl_loc : Location) : Ast::Import? # TODO: Define Ast::Import
    @log.debug(decl_loc, "Parsing import declaration...")
    # Pony: parse_import(loc)
    raise report_error(decl_loc, "parse_import not yet implemented")
    nil # Placeholder
  end

  def parse_extend(decl_loc : Location) : Ast::Extend?
    @log.debug_descend(decl_loc, "Parsing extend declaration...") do
      type_params = parse_type_parameters?
      type = parse_type_expression
      traits = parse_traits? || [] of Ast::Type
      extension = Ast::Extend.new(decl_loc, type_params, type, traits)
      @declarations << extension
      receiver = Ast::Parameter.new(decl_loc, "self", type)
      until eof?
        break if skip_newline_unless_block_end?(decl_loc.indent)
        if str_tok = consume?(Token::String)
          @log.info(str_tok.location, "Skipping docstring:\n#{str_tok.data}")
          next
        end
        key_tok = consume!(Token::Variable)
        case key_tok.data
        when "field"
          report_error(key_tok.location, "Type extensions cannot add fields, only methods.")
          parse_field(key_tok.location)
        when "def"
          parse_method(key_tok.location, receiver, type_params)
        when "constructor"
          @declarations << parse_constructor(key_tok.location, type, type_params)
        when "static"
          @declarations << parse_static(key_tok.location, type.name, type_params)
        else
          report_error(peek.location, "Unexpected #{peek.data} in extend declaration")
          break
        end
      end
      extension
    end
  end
end

abstract struct SubParser
  property parser : Parser
  def initialize(@parser : Parser)
  end
  def peek
    parser.peek
  end
  def peek2
    parser.peek2
  end
  def next_token
    parser.next_token
  end
  def report_error(loc, msg)
    parser.report_error(loc, msg)
  end
  def consume_identifier
    parser.consume_identifier
  end
  def consume!(tok)
    parser.consume!(tok)
  end
  def consume?(tok)
    parser.consume?(tok)
  end
  def log
    parser.log
  end
end

# ExpressionParser is a subparser that parses an expression.
# The following properties are used to detect the end of an expression:
# - expression_indent: the indent of the first token in the expression
#   - a token with indent not greater than this ends the expression
# - line_indent: the actual indent of the current line
#   - this becomes the block_indent of the next `if`, `for`, or `while` block
struct ExpressionParser < SubParser
  property terms : Array(Ast::Expr) = [] of Ast::Expr
  property operators : Array(Operator) = [] of Operator
  getter expression_indent : UInt32
  property stop : StopAt
  property expecting : Expecting = Expecting::Term
  
  def initialize(
    @parser : Parser, 
    @expression_indent : UInt32,
    @stop : StopAt)
  end
  def initialize(@parser : Parser, @stop : StopAt = StopAt::Normal)
    @expression_indent = peek.location.indent
  end
  def initialize(@parser : Parser, @expression_indent : UInt32, @stop : StopAt = StopAt::Normal)
  end
  
  def parse : Ast::Expr
    log.debug_descend(peek.location, "parsing expression(#{stop})...") do
      if expression_done?
        log.debug(peek.location, "empty expression (peek: #{peek})")
        parser.consume?(Token::Colon)  # to avoid infinite loop
        raise report_error(peek.location, "Empty expression")
        # return Ast::EmptyNode.new(peek.location)
      end
      
      until expression_done?
        case @expecting
        in Expecting::Term
          read_operand
        in Expecting::Operator
          if read_operator == :done
            break
          end
        end
      end

      if @expecting == Expecting::Term
        raise report_error(peek.location, "expected operand after #{operators.last {"<noop>"}}")
      else
        log.debug(peek.location, "terms before reduction: #{terms.join(", ")}")
        log.debug(peek.location, "operators before reduction: #{operators.join(", ")}")
      end
      
      while operators.size > 0
        compress
      end
      if operators.size != 0 || terms.size != 1
        report_error(peek.location, "operators and terms did not add up")
      else
        log.debug(peek.location, "Successfully parsed expression: #{terms[0]}; next: #{peek}")
      end
      terms.pop
    end
  end

  def expect(expected : Expecting)
    @expecting = expected
  end
  
  def push_operator(right_op : Operator)
    log.debug_descend(peek.location, "pushing operator: #{right_op}") do
      while operators.size > 0
        left_op = operators.last
        log.debug(peek.location, "left_op #{left_op} binds #{left_op.relative_precedence(right_op)} then right_op #{right_op}")
        case left_op.relative_precedence(right_op)
        when Bind::None
          report_error(peek.location, "Relative precedence not defined between #{left_op} [left] and #{right_op} [right].  Disambiguate with parentheses.")
          compress
        when Bind::Tighter
          compress
        when Bind::Looser
          log.debug(peek.location, "put #{right_op} on stack")
          break
        end
      end
      operators << right_op
    end
  end
  
  def compress
    return if operators.empty?
    log.debug(peek.location, "compressing: operators: #{operators.join(", ")}; terms: #{terms.join(", ")}")
    case op = operators.pop
    when Operator::Neg
      t1 = pop_term!
      terms << Ast::NegNode.new(t1.location, t1)
    when Operator::Not
      t1 = pop_term!
      terms << Ast::NotNode.new(t1.location, t1)
    when Operator::Comma
      t2 = pop_term!
      t1 = pop_term!
      tuple_terms = [] of Ast::Expr
      if t1.is_a?(Ast::Tuple)
        tuple_terms.concat(t1.elements)
      else
        tuple_terms << t1
      end
      if t2.is_a?(Ast::Tuple)
        tuple_terms.concat(t2.elements)
      else
        tuple_terms << t2
      end
      terms << Ast::Tuple.new(tuple_terms)
    when Operator
      t2 = pop_term!
      t1 = pop_term!
      terms << Ast::Binop.new(t1, op, t2)
    end
    log.debug(peek.location, "compressed: operators: #{operators.join(", ")}; terms: #{terms.join(", ")}")
  end
  
  def pop_term!
    terms.pop
  rescue
    raise report_error(peek.location, "Not enough terms to complete expression")
  end

  def last_term(&default)
    terms[terms.size - 1]? || yield
  end
  
  def last_term!
    terms[terms.size - 1]
  rescue
    raise report_error(peek.location, "Not enough terms to complete expression")
  end

  # Given the stop conditions of the current sub-parser, return whether or not
  # the next token will force the sub-parser is forced to stop.
  def expression_done?
    case tok = peek
    when Token::RParen, Token::RBracket, Token::RBrace, Token::Else, Token::Colon, Token::EOF
      true
    when Token::Newline
      tok.data <= expression_indent
    else
      case {stop, tok}
      when {StopAt::UcsBranch, _}
        if (tok.ucs_branch_token? && (newline_tok = peek2).is_a?(Token::Newline)) ||
          (peek2.ucs_branch_token? && (newline_tok = tok).is_a?(Token::Newline))
          newline_tok.data > expression_indent
        else
          false
        end
      when {StopAt::UcsEnd, Token::Operator}
        tok.data == Operator::And && 
          (newline_tok = peek2).is_a?(Token::Newline) && 
          newline_tok.data > expression_indent
      when {StopAt::Comma, Token::Comma}
        true
      when {StopAt::Newline, Token::Newline}
        true
      else
        false
      end
    end
  end  
  
  def read_operand
    log.debug_descend(peek.location, "read_operand: #{peek}") do
      term = case (tok = next_token)
      when Token::Operator
        case {tok.data, num = peek}
        when {Operator::Sub, Token::Int}
          next_token
          Ast::Int.new(tok.location, num.data * -1)
        when {Operator::Sub, Token::Float}
          next_token
          Ast::Float.new(tok.location, num.data * -1.0)
        when {Operator::Sub, _}
          push_operator(Operator::Neg)
          return read_operand
        else
          raise report_error(tok.location, "Expected term or prefix operator.  Got #{tok}")
        end
      when Token::Not
        push_operator(Operator::Not)
        return read_operand
      when Token::KeyWord
        case tok.data
        # when KeyWord::Let then handle_arg_passing_mode(tok.location, "let")
        # when KeyWord::Mut then handle_arg_passing_mode(tok.location, "mut")
        # when KeyWord::Move then handle_arg_passing_mode(tok.location, "move")
        # when KeyWord::Copy then handle_arg_passing_mode(tok.location, "copy")
        # when KeyWord::Ref then handle_arg_passing_mode(tok.location, "ref")
        else
          parse_control_statement(tok.location, tok.data)
        end

      when Token::Variable
        if peek.is_a?(Token::LParen | Token::LBracket)
          function_call(tok.location, tok.data)
        else
          parse_word(tok.location, tok.data)
        end
      when Token::Type
        handle_type(tok.location, tok.data)
      when Token::LParen
        parse_paren_group(tok.location)
      when Token::LBracket
        parse_array_literal(tok.location)
      when Token::Int then Ast::Int.new(tok.location, tok.data)
      when Token::Float then Ast::Float.new(tok.location, tok.data)
      when Token::String then Ast::String.new(tok.location, tok.data)
      when Token::Dot
        terms << Ast::Identifier.new(tok.location, "self")  # implicit self-arg
        handle_dot()
      when Token::Error
        raise report_error(tok.location, tok.data)
      else
        raise report_error(tok.location, "Expected term or prefix operator.  Got #{tok}")
      end
      terms << term
      expect(Expecting::Operator)
      log.debug(tok.location, "pushed term: #{term}, now reading operator...")
    end
  end

  # Reads the next operator and pushes it onto the stack, then moves on to the operand
  # Peek the next token, and if it ends the expression, leave it there and return
  def read_operator
    log.debug_descend(peek.location, "next operator: #{peek}") do  
      expect(
        case tok = peek
        when Token::Operator
          next_token
          push_operator(tok.data)
          Expecting::Term
        when Token::Comma
          next_token
          push_operator(Operator::Comma)
          Expecting::Term
        when Token::Dot
          next_token
          terms << handle_dot()
          Expecting::Operator
        when Token::LParen, Token::LBracket
          raise report_error(tok.location, "only named functions are supported, cannot 'call' arbitrary expressions")
          # otherwise this would have been caught in read_operand
        when Token::Error
          raise report_error(next_token.location, tok.data)
        else
          if stop == StopAt::ExpressionEnd
            log.debug(tok.location, "stop is StopAt::ExpressionEnd; we're finished here; next: #{peek}")
            return :done
          elsif tok.is_a?(Token::Newline)
            # TODO: logic for continuing after newline
            return :done
          else
            next_token
            raise report_error(tok.location, "Expected binary operator; got #{tok}")
          end
        end
      )
    end
  end
    
  def parse_control_statement(loc : Location, word : KeyWord)
    case word
    when KeyWord::Let then parse_let_declaration(loc)
    when KeyWord::Var then parse_var_declaration(loc)
    when KeyWord::Const then parse_const_declaration(loc)
    # when KeyWord::Mut 
    #   read_operand

    
    when KeyWord::If then parse_if_expression(loc)
    when KeyWord::While then parse_while_expression(loc)
    when KeyWord::For then parse_for_expression(loc)
    when KeyWord::Return then Ast::Return.new(loc, ExpressionParser.new(parser, expression_indent, stop).parse)
    when KeyWord::Break then Ast::Break.new(loc)
    when KeyWord::Continue then Ast::Continue.new(loc)
    else
      raise report_error(loc, "Expected term or prefix operator.  Got wierd control word: #{word}")
    end
  end

  def parse_word(loc : Location, word : String)
    case word
    when "true" then Ast::True.new(loc)
    when "false" then Ast::False.new(loc)
    when "nil" then Ast::Nil.new(loc)
    when "let", "mut", "move", "copy", "ref"
      # handle_arg_passing_mode(loc, word)
      raise report_error(loc, "Argument passing mode not yet implemented.")
    else
      Ast::Identifier.new(loc, word)
    end
  end
  
  def parse_paren_group(loc : Location)
    log.debug_descend(loc, "parse_paren_group") do
      expr = ExpressionParser.new(parser, expression_indent, stop).parse
      consume?(Token::RParen) || raise report_error(peek.location, "Expected closing parenthesis.")
      expr
      # if statements.size == 1
      #   statements[0]
      # else
      #   Ast::Block.new(loc, consume statements)
      # end
    end
  end
  
  def parse_array_literal(loc : Location)
    log.debug_descend(loc, "parse_array_literal") do
      list_indent = parser.current_line_indent
      parser.skip_newline_unless_block_end?(list_indent, "Expected array element; got end of block.")
      if consume?(Token::RBracket)
        return Ast::EmptyArray.new(loc)
      end
      statements = parse_list
      consume!(Token::RBracket)  # || raise report_error(peek.location, "Expected closing bracket.")
      if statements.size == 0
        log.warning(loc, "empty array literal")
        Ast::EmptyArray.new(loc)
      else
        log.debug(loc, "array literal with #{statements.size} elements")
        Ast::Array.new(loc, statements)
      end
    end
  end

  def handle_type(loc : Location, type : String)
    type_node = Ast::Type.new(loc, type, parser.parse_type_args?)
    if consume?(Token::Dot)
      log.debug(loc, "static method")
      name = consume_identifier
      Ast::StaticCall.new(loc, type_node, function_call(loc, name))
    else
      Ast::Constructor.new(loc, type_node, parse_args)
    end
  end
  
  def handle_dot
    log.debug_descend(peek.location, "handle_dot") do
      node = pop_term!
      name = consume_identifier
      function_call(node.location, name, node)
    end
  end

  def function_call(loc : Location, name : String)
    type_args = parser.parse_type_args?
    args = parse_args
    Ast::Call.new(loc, name, type_args, args)
  end
  def function_call(loc : Location, name : String, receiver : Ast::Expr)
    type_args = parser.parse_type_args?
    args = parse_args([receiver])
    Ast::Call.new(loc, name, type_args, args)
  end
  
  def parse_args(args : Ast::Args = Ast::Args.new) : Ast::Args
    log.debug_descend(peek.location, "parsing args...") do
      unless consume?(Token::LParen)
        return args
      end
      parser.skip_newline_unless_block_end?(@expression_indent)
      if consume?(Token::RParen)
        return args
      end
      list = parse_list(args)
      consume!(Token::RParen)
      list
    end
  end

  def parse_list(exprs : Array(Ast::Expr) = [] of Ast::Expr) : Array(Ast::Expr)
    until expression_done?
      # convention = parser.read_convention?
      exprs << ExpressionParser.new(parser, @expression_indent, StopAt::Comma).parse
      parser.consume_list_separator?(@expression_indent) || break
    end
    log.debug(peek.location, "done parsing list: #{exprs}")
    exprs
  end
  
  def parse_let_declaration(loc : Location)
    name = consume_identifier
    consume!(Operator::Assign)
    value = ExpressionParser.new(parser, expression_indent, stop).parse
    Ast::Let.new(loc, name, value)
  end

  def parse_var_declaration(loc : Location)
    name = consume_identifier
    consume!(Operator::Assign)
    value = ExpressionParser.new(parser, expression_indent, stop).parse
    Ast::Var.new(loc, name, value)
  end
    
  def parse_const_declaration(loc : Location)
    name = consume_identifier
    consume!(Operator::Assign)
    value = ExpressionParser.new(parser, expression_indent, stop).parse
    Ast::Const.new(loc, name, value)
  end
    
  def parse_if_expression(loc : Location)
    conditionals = UcsParser.new(parser).parse
    Ast::If.new(loc, conditionals)
  end
  
  def parse_while_expression(loc : Location)
    condition = ExpressionParser.new(parser, expression_indent).parse
    body = parser.parse_colon_and_block
    Ast::WhileLoop.new(loc, condition, body)
  end
  
  def parse_for_expression(loc : Location)
    var_loc = peek.location
    var_name = consume!(Token::Variable).data.as(String)
    unless consume?(Operator::In)
      report_error(peek.location, "Expected \"in\" after variable name in for loop; got #{peek}")
    end
    iterator = ExpressionParser.new(parser, expression_indent).parse
    body = parser.parse_colon_and_block
    Ast::ForLoop.new(loc, Ast::Identifier.new(var_loc, var_name), iterator, body)
  end
end

struct UcsParser < SubParser
  def initialize(@parser : Parser)
  end
  def parse(condition_indent : UInt32 = parser.current_line_indent) : Array(Ast::Condition)
    loc = peek.location
    conditions = [] of Ast::Condition
    log.debug_descend(loc, "parsing ucs...") do
      indent = condition_indent
      @parser.tree_branch(loc, indent) do |indented|
        # PARSE CONDITIONAL
        indent = peek.location.indent if indented

        # check for 'else' token
        if else_tok = consume?(Token::Else)
          if conditions.size == 0
            raise report_error(peek.location, "Expected conditional after `if`, `while`, or `and` token.")
          end
          consequent = parser.parse_colon_and_block(indent)
          conditions << Ast::Condition.else(else_tok.location, consequent)
          return conditions
        end

        lhs = parser.parse_expression(indent, StopAt::UcsBranch)
        
        # check for unary condition
        if peek.is_a? Token::Colon
          consequent = parser.parse_colon_and_block(indent)
          conditions << Ast::Condition.unary(lhs.location, lhs, consequent)
          if else_tok = consume?(Token::Else)
            consequent = parser.parse_colon_and_block(indent)
            conditions << Ast::Condition.else(else_tok.location, consequent)
            return conditions
          end
          next
        elsif consume?(Operator::And)
          nested = UcsParser.new(parser).parse(indent)
          conditions << Ast::Condition.unary(lhs.location, lhs, nested)
          next
        end
        
        # continue with binary condition
        conditions << Ast::BinaryCondition.new(lhs.location, lhs, ops = [] of Ast::OpBranch)

        @parser.tree_branch(peek.location, indent) do |indented|
          # PARSE BOOLEAN OPERATOR
          indent = peek.location.indent if indented
          if else_tok = consume?(Token::Else)
            raise report_error(peek.location, "Expected boolean operator after lhs #{lhs}") if ops.size == 0
            consequent = parser.parse_colon_and_block(indent)
            conditions << Ast::Condition.else(else_tok.location, consequent)
            return conditions
          end
          op_tok = consume!(Token::Operator).as(Token::Operator)
          ops << Ast::OpBranch.new(op_tok.location, op_tok.data, term_split = [] of Ast::RTerm)
          
          @parser.tree_branch(peek.location, indent) do |indented|
            # PARSE RHS
            indent = peek.location.indent if indented
            if else_tok = consume?(Token::Else)
              raise report_error(peek.location, "Expected right-hand term after operator #{op_tok}") if term_split.size == 0
              consequent = parser.parse_colon_and_block(indent)
              conditions << Ast::Condition.else(else_tok.location, consequent)
              return conditions
            end
            rhs = parser.parse_expression(indent, StopAt::UcsEnd)
            if peek.is_a? Token::Colon
              consequent = parser.parse_colon_and_block(indent)
              term_split << Ast::RTerm.new(rhs.location, rhs, consequent)
            elsif consume?(Operator::And)
              nested = UcsParser.new(parser).parse(indent)
              term_split << Ast::RTerm.new(rhs.location, rhs, nested)
            else
              raise report_error(peek.location, "Expected colon or 'and' after test #{rhs}; got #{peek}")
            end
          end
          raise report_error(peek.location, "Expected right-hand term after operator #{op_tok}") if term_split.size == 0
        end
        raise report_error(peek.location, "Expected boolean operator after lhs #{lhs}") if ops.size == 0
      end
      raise report_error(peek.location, "Expected conditional after `if`, `while`, or `and` token.") if conditions.size == 0
    end

    # check if token on next line is 'else'
    if (tok = peek).is_a?(Token::Newline)
      if tok.data == condition_indent
        # yuck... but I don't know how else to peek ahead *two* tokens without upsetting the lexer
        # if parser.lexer.reader.peek_str?("else:") || parser.lexer.reader.peek_str?("else ")
        if parser.peek2.class == Token::Else
          log.warning(tok.location, "found 'else' on newline")
          next_token             # newline
          else_tok = next_token  # else
          raise "OOPS" unless else_tok.class == Token::Else
          consequent = parser.parse_colon_and_block
          conditions << Ast::Condition.else(else_tok.location, consequent)
        end
      end
    end
    conditions
  end        
end

class Parser
  # property lexer : Lexer
  # @peeked : Token? = nil
  property current_line_indent : UInt32 = 0  # indentation of the first token of the current expression, or if, struct, trait, def, etc
  property log : Logger
  getter declarations = [] of Ast::TopLevelItem # The final list of top-level AST nodes

  # --- Exception for Parse Errors to aid in recovery ---
  class ParseError < Exception
    getter location : Location
    def initialize(message : String, @location : Location)
      super("#{@location}: #{message}")
    end
  end

  def initialize(@tokens, @log, @declarations = [] of Ast::TopLevelItem)
  end
  def initialize(lexer : Lexer, @log : Logger, @declarations = [] of Ast::TopLevelItem)
    @tokens = lexer.lex
  end
  def initialize(text : String, file_path : String, starting_line, log_level : Logger::Level = Logger::Level::Warning)
    @log = Logger.new(log_level)
    @tokens = Lexer.new(Lexer::Reader.new(text, file_path, starting_line), @log).lex
  end

  # == Main Entry Point ==
  def parse : Array(Ast::TopLevelItem)
    @log.info(Location.zero, "Parser.parse start")

    until eof?
      begin
        if peek.is_a?(Token::String)
          @log.info(peek.location, "Skipping docstring:\n#{next_token.data}")
        else
          parse_top_level_item
        end
        # parse_top_level_item is expected to add items to @declarations directly
        # if it parses a construct that can produce multiple items (like 'function' block).
        # Otherwise, it returns a single item.
        # if item = parse_top_level_item
        #   @declarations << item
        # end
        consume?(Token::Newline)
      rescue err : ParseError
        # Logged by report_error, here we just ensure we can continue to next top-level item
        @log.debug(err.location, "Caught ParseError: #{err.message}, attempting to find next top-level item.")
        recover_to_next_declaration
      end
    end

    @log.info(peek.location, "Parser.parse end, #{@declarations.size} declarations found.")
    @declarations
  end

  include TokenNavigation
  include TopLevelItemParser

  def parse_expression(expression_indent : UInt32, stop_at : StopAt = StopAt::Normal)
    ExpressionParser.new(self, expression_indent, stop_at).parse
  end
  def parse_expression(stop : StopAt = StopAt::Normal)
    ExpressionParser.new(self, peek.location.indent, stop).parse
  end

  # perform a block of code once if the parser finds a single line of code, 
  # or multiple times if it finds indented items
  # `loc` is the location of the beginning of the expression/definition
  def tree_branch(loc : Location, &block)
    block_indent = loc.indent
    log.debug_descend(loc, "tree_branch (block_indent: #{block_indent})...") do
      if consume_indent?(block_indent)
        until eof?
          res = yield true
          break res if skip_newline_unless_block_end?(block_indent)
        end
      else
        yield false
      end
    end
  end

  def tree_branch(loc : Location, block_indent : UInt32, &block)
    log.debug_descend(loc, "tree_branch (block_indent: #{block_indent})...") do
      if consume_indent?(block_indent)
        until eof?
          res = yield true
          break res if skip_newline_unless_block_end?(block_indent)
        end
      else
        yield false
      end
    end
  end
end