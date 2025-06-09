require "./tokens"
require "./ast_nodes"
require "./logger" 


# Placeholder for operator precedence comparison
# This would be defined elsewhere, e.g., in a file for operator logic.
# enum PrecedenceResult
#   Tighter # Left operator has tighter precedence (evaluate left first)
#   Looser  # Left operator has looser precedence (evaluate right first / push right)
#   Equal   # Equal precedence (usually left-associative)
#   None    # Precedence not comparable (error or specific handling)
# end
# def operator_compare(op_left : Operator, op_right : Operator) : PrecedenceResult
#   # ... implementation needed ...
#   PrecedenceResult::None # Placeholder
# end

# Defines the types of tokens that can stop an expression parser
enum StopAt
  Normal             # Default: stops at block closers, major separators like Colon, EOF
  BooleanOperator    # Stops if the next token is a boolean comparison operator
  ColonOrAnd         # Stops at Colon or an 'and' keyword/operator
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
  # Peeks at the current token without consuming
  def peek : Token
    @peeked ||= @lexer.next(@context)
  end

  # Consumes the current token and returns it. Advances the index.
  # Returns the last token if already at EOF to prevent errors on multiple calls.
  def next_token : Token
    if tok = @peeked
      @peeked = nil
      tok
    else
      @lexer.next(@context)
    end
  end

  # Checks if the parser has reached the end of the token stream.
  def eof? : Bool
    peek.is_a?(Token::EOF)
  end

  def with_context(ctx : ParserContext, &block)
    old_ctx = @context
    @context = ctx
    yield
  ensure
    # Crystal parser type-checks `old_ctx` as nilable, because the assignment
    # occurs in the begin block.  However, we know it can't be nil here, so this is safe.
    @context = old_ctx || ParserContext::TopLevel
  end

  # Reads an optional convention keyword (let, mut, move, copy, ref)
  def read_convention? : Convention # Convention = Mode | Nil
    tok = peek
    if tok.is_a?(Token::KeyWord)
      case tok.data
      when KeyWord::Let   then next_token; Mode::Let
      # TODO: Add other keywords that map to Mode enum (mut, move, copy, ref)
      # when KeyWord::Mut   then next_token; Mode::Mut
      # when KeyWord::Move  then next_token; Mode::Move
      # when KeyWord::Copy  then next_token; Mode::Copy
      # when KeyWord::Ref   then next_token; Mode::Ref
      else 
        nil  # Not a convention keyword
      end
    # Pony also allowed string matches for "mut", "move", etc.
    # If these are lexed as VariableName tokens but act as conventions:
    elsif tok.is_a?(Token::Variable)
      mode = Mode.from_string?(tok.data)
      next_token if mode
      mode
    else
      nil # No convention keyword found
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
    # if ignore_newline
    #   if peek(:skip_newline).is_a?(Token::Type)
    #     next_token(:skip_newline).as(Token::Type).data
    #   end
    # else
      if (tok = peek).is_a?(Token::Type)
        next_token.as(Token::Type).data
      end
    # end
  end

  # --- Error Reporting & Recovery ---
  def report_error(location : Location, message : String) : Parser::ParseError
    @log.error(location, message)
    Parser::ParseError.new(message, location)
  end

  def recover_to_next_declaration
    # Simple recovery: skip tokens until we find a keyword that typically starts a new declaration
    @context = ParserContext::TopLevel
    @log.debug(peek.location, "Attempting recovery...")
    until eof?
      tok = peek
      # Check for top-level keywords
      if tok.is_a?(Token::KeyWord)
        case tok.data
        when KeyWord::Struct, KeyWord::Enum, KeyWord::Trait, 
             KeyWord::Extend, KeyWord::Function, KeyWord::Def, KeyWord::Import
          @log.debug(tok.location, "Recovery found keyword #{tok.data}, resuming parse.")
          return
        end
      end
      next_token # Consume and discard the token
    end
    @log.debug(peek.location, "Recovery reached EOF or skipped too many tokens.")
  end

  def recover_to_next_def_or_sub_item
    @context = ParserContext::TopLevel
    @log.debug(peek.location, "Attempting recovery to next sub-item...")
    until eof?
      if peek.is_a?(Token::KeyWord)
        case peek.data
        when KeyWord::Struct, KeyWord::Enum, KeyWord::Trait, 
             KeyWord::Extend, KeyWord::Function, KeyWord::Import,
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

module TopLevelItemParser
  def parse_top_level_item : Ast::TopLevelItem?
    loc = peek.location
    @log.debug_descend(loc, "Parsing top-level item, current token: #{peek}") do
      keyword_token = peek
      unless keyword_token.is_a?(Token::KeyWord)
        raise report_error(keyword_token.location, "Expected top-level declaration (struct, function, enum, etc.); got #{keyword_token}")
      end

      # Now we know it's a KeyWord token, get its data
      keyword_data = keyword_token.data.as(KeyWord)
      next_token # Consume the keyword token itself

      case keyword_data
      when KeyWord::Function
        # parse_function_block will add Ast::Function nodes directly to @declarations
        parse_function_block(loc)
      when KeyWord::Struct
        parse_struct(loc)
      when KeyWord::Trait
        parse_trait(loc)
      when KeyWord::Enum
        parse_enum(loc)
      when KeyWord::Import
        raise "This should have been handled in the lexer"
      when KeyWord::Extend
        parse_extend(loc)
      else
        raise report_error(loc, "Unexpected keyword for top-level declaration: '#{keyword_data}'")
      end
    end
  end

  # --- Function Parsing ---
  # Handles `function NAME ... def ... end`
  # Adds Ast::Function nodes directly to @declarations.
  def parse_function_block(function_keyword_loc : Location)
    block_name = consume_identifier("function block name")

    @log.info(function_keyword_loc, "Parsing function block '#{block_name}'")
    @log.debug_descend do
      type_params = parse_type_parameters?

      # check if this is a single function or a function block
      case peek
      when KeyWord::Def
        # continue
      when Token::LBrace, Token::Type, Token::Colon
        @declarations << parse_def_overload(function_keyword_loc, block_name, type_params)
        return
      else
        raise report_error(peek.location, "Expected 'def' or function signature after function block name")
      end

      until eof?
        break unless def_tok = consume?(KeyWord::Def)
        
        if overload_ast = parse_def_overload(def_tok.location, block_name, type_params)
          @declarations << overload_ast
        else
          # Error in parsing overload, already reported. Attempt to recover to next 'def' or end of block.
          recover_to_next_def_or_sub_item
        end
      end
    end
    @log.info(peek.location, "Finished parsing function block '#{block_name}'")
  end

  # Parses a single `def [TYPE_PARAMS]? (PARAMS)? RETURN_TYPE : BODY`
  # within a function block
  def parse_def_overload(def_loc : Location, name : String, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function
    @log.debug_descend(def_loc, "Parsing def for '#{name}'") do
      sig = parse_signature(peek.location, inherited_type_params)
      with_context(ParserContext::Block) do
        body = parse_colon_and_block(def_loc.column)
        consume?(Token::Newline) || report_error(peek.location, "Expected newline after function body")
        Ast::Function.new(def_loc, name, sig, body)
      end
    end
  end

  # Parses a `def CONVENTION? NAME [TYPE_PARAMS]? (PARAMS)? RETURN_TYPE : BODY`
  # within a struct or enum block
  # The syntax sugar `def .method` is parsed as `def method(self SelfType)`
  # and `def mut.method(mut self SelfType)` is parsed as `def method(mut self SelfType)`
  def parse_def_method(name : String?, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function
    def_loc = peek.location
    parse_def_overload(def_loc, name, inherited_type_params)
  end

  def parse_abstract_def_method(name : String?, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function
    def_loc = peek.location
    parse_def_overload(def_loc, name, inherited_type_params)
  end

  def parse_signature(sig_loc : Location, 
                      inherited_type_params : Array(Ast::TypeParameter)? = nil,
                      receiver : Ast::Parameter? = nil, 
                     ) : Ast::Signature?
    @log.debug_descend(sig_loc, "Parsing signature...") do
      type_params = parse_type_parameters?(inherited_type_params)
      params : Array(Ast::Parameter)? = if receiver 
        parse_parameters([receiver])
      else
        parse_parameters?
      end
      return_type = 
        # optional right-arrow, optional convention, optional return type
        if consume?(Operator::RArrow) || (convention = read_convention?) || peek.is_a?(Token::Type)
          parse_type_expression
        else
          nil
        end
      Ast::Signature.new(sig_loc, type_params, params, return_type, convention)
    end
  end

  # Parses `[T, U: Constraint]` or `[A, B: Constraint1 & Constraint2]`
  def parse_type_parameters?(inherited_type_params : Array(Ast::TypeParameter)? = nil) : Array(Ast::TypeParameter)?
    type_params = inherited_type_params.try &.dup
    return type_params unless peek.is_a?(Token::LBracket)
    
    type_params ||= [] of Ast::TypeParameter
    lbracket_loc = next_token.location  # consume '['
    if consume?(Token::RBracket)
      return type_params
    end
    
    @log.debug(peek.location, "peek=#{peek}")  # to skip the possible leading newline before entering into list context

    @log.debug_descend(lbracket_loc, "Parsing type parameters...") do
      with_context ParserContext::List do
        until eof?
          # parse type parameter
          param_loc = peek.location
          name = consume_type_name!
          constraints = parse_constraints
          type_params << Ast::TypeParameter.new(param_loc, name, constraints)
          unless consume?(Token::Comma) 
            break if peek.is_a?(Token::RBracket)
            raise "Expected ',' or ']' after type parameter; got #{peek}"  
          end
          break if peek.is_a?(Token::RBracket)
        end
        consume!(Token::RBracket)
        type_params
      end
    end
  end

  # UNUSED; this was gonna be a wrapper function for parsing type parameters, 
  # type args, traits, and constraints
  def parse_list(delimiter : (Token::Comma.class | Operator::And.class), &block)
    peek # to skip the possible leading newline before entering into list context
    until eof?
      yield block
      consume?(delimiter) || break
    end
    consume!(delimiter)
  end

  # Parses `: Trait & Trait[Int]`
  def parse_traits?
    # consume optional colon or `is`
    return nil unless consume?(Token::Colon) || consume?(Operator::Is)
    @log.debug_descend(peek.location, "Parsing traits...") do
      # contraint parsing may occur in a list context, but we still want to require
      # the `&` separator yet ignore newlines, so we use ParserContext::TopLevel
      with_context ParserContext::TopLevel do
        traits = [] of Ast::Type
        until eof?
          traits << parse_type_expression
          consume?(Operator::And) || break
        end
        traits
      end
    end
  end

  # Parses `: Trait & Trait[Int]`
  def parse_constraints : Ast::Constraints
    constraints = Ast::Constraints.new
    unless consume?(Token::Colon) || consume?(Operator::Is) || peek.data == Operator::Not || peek.is_a?(Token::Type)
      return constraints
    end
    @log.debug_descend(peek.location, "Parsing constraints...") do
      with_context ParserContext::TopLevel do
        # parse first constraint
        if consume?(Operator::Not)
          constraints.exclude(parse_type_expression)
        else
          constraints.include(parse_type_expression)
        end
        # parse remaining constraints
        while true
          if consume?(Operator::And)
            constraints.include(parse_type_expression)
          elsif consume?(Operator::Not)
            constraints.exclude(parse_type_expression)
          else
            break
          end
        end
      end
    end
    constraints
  end

  # macro parse_parameters(receiver)
  #   {% unless receiver.is_a?(ArrayLiteral) && receiver.size <= 1 %}
  #     {% raise "Receiver must be an array-literal of zero or one parameters." %}
  #   {% end %}
  #   parse_parameters_impl({{receiver}})
  # end

  def parse_parameters(params : Array(Ast::Parameter))
    return params unless consume?(Token::LParen)
    return params if consume?(Token::RParen)
    
    peek  # to skip the possible leading newline before entering into list context

    @log.debug_descend(peek.location, "Parsing parameters...") do
      with_context ParserContext::List do
        until eof?
          params << parse_parameter
          consume?(Token::Comma) || break
        end
        consume!(Token::RParen)
        params
      end
    end
  end
  
  # Parses (p1: Type, p2: Type)
  def parse_parameters? : Array(Ast::Parameter)?
    return nil unless peek.is_a?(Token::LParen)
    parse_parameters([] of Ast::Parameter)
  end

  def parse_parameter : Ast::Parameter
    loc = peek.location
    convention = read_convention?
    name = consume_identifier("parameter name")
    # optional colon
    consume?(Token::Colon)
    type = parse_type_expression
    Ast::Parameter.new(loc, convention, name, type)
  end

  # Parses a type expression, e.g., MyType, MyGenericType[Arg1, Arg2]
  def parse_type_expression : Ast::Type
    loc = peek.location
    name = consume_type_name!
    @log.debug_descend(loc, "Parsing type expression: #{name}") do
      type_args = parse_type_args?
      Ast::Type.new(loc, name, type_args)
    end
  end

  # Parses type arguments, e.g., [Arg1, Arg2]
  def parse_type_args? : Array(Ast::Type)?
    consume?(Token::LBracket) || return nil
    type_args = [] of Ast::Type
    return type_args if consume?(Token::RBracket)

    peek  # to skip the possible leading newline before entering into list context
    
    @log.debug_descend(peek.location, "Parsing type arguments...") do
      with_context ParserContext::List do
        until eof?
          type_args << parse_type_expression
          consume?(Token::Comma) || break
        end
        consume!(Token::RBracket)
        type_args
      end
    end
  end

  # Parses `: BLOCK_OF_STATEMENTS`
  def parse_colon_and_block(block_base_indent : UInt32)
    loc = peek.location
    @log.debug_descend(loc, "Parsing colon and block (base indent: #{block_base_indent})...") do
      consume!(Token::Colon)

      # Statements are parsed relative to the block_base_indent.
      # A statement belongs to the block if it's indented further than block_base_indent.
      with_context ParserContext::Block do
        statements = [] of Ast::Expr
        until eof?
          @log.debug(peek.location, "Parsing statement starting with #{peek}")
          case tok = peek
          when Token::Newline
            break if tok.data <= block_base_indent
            next_token
          when Token::EOF, Token::RBrace, Token::RParen, Token::RBracket
            break
          else
            break if tok.location.column <= block_base_indent
            statements << parse_expression(peek.location.indent)
          end
        end
        statements
      end
    end
  end
  
  # Parses a sequence of statements until indent decreases or block ends
  def parse_statements(current_block_indent : UInt32, delimiter : StopAt = StopAt::Normal) : Array(Ast::Expr)
    @log.debug_descend(peek.location, "parse_statements (indent: #{current_block_indent}, stop: #{delimiter}) - NOT FULLY IMPLEMENTED") do
      statements = [] of Ast::Expr
      until eof?
        @log.debug(peek.location, "Parsing statement starting with #{peek}")
        case tok = peek
        when Token::Newline
          break if tok.data <= current_block_indent
          next_token
        when Token::EOF, Token::RBrace, Token::RParen, Token::RBracket
          break
        else
          break if tok.location.column <= current_block_indent
          statements << parse_expression(peek.location.indent)
        end
      end
      statements
    end
  end

  def parse_type_header : {Convention, String, Array(Ast::TypeParameter)?, Array(Ast::Type)?}
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
        if peek.location.column <= decl_loc.column
          break
        end
        key_tok = consume!(Token::KeyWord)
        case key_tok.data
        when KeyWord::Field
          struct_dec.fields << parse_field(key_tok.location)
        when KeyWord::Def
          @declarations << parse_method(key_tok.location, receiver, type_params)
        when KeyWord::Constructor
          @declarations << parse_constructor(key_tok.location, receiver.type, type_params)
        when KeyWord::Static
          @declarations << parse_static(key_tok.location, name, type_params)
        else
          report_error(peek.location, "Unexpected token in struct declaration")
          break
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
  def parse_method(loc : Location, receiver : Ast::Parameter, inherited_type_params : Array(Ast::TypeParameter)? = nil) : Ast::Function
    @log.debug_descend(loc, "Parsing method with receiver: #{receiver}, inherited type params: #{inherited_type_params}") do
      convention = read_convention?
      receiver.convention = convention if convention
      name = consume_identifier
      signature = parse_signature(peek.location, inherited_type_params, receiver)
      indent = loc.column == 0 ? 0_u32 : loc.column - 1
      block = parse_colon_and_block(indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after method body")
      Ast::Function.new(loc, name, signature, block)
    end
    
  end

  def parse_abstract_method(loc : Location, receiver : Ast::Parameter, inherited_type_params : Array(Ast::TypeParameter)? = nil) : Ast::AbstractMethod
    @log.debug_descend(loc, "Parsing abstract method with receiver: #{receiver}, inherited type params: #{inherited_type_params}") do
      convention = read_convention?
      receiver.convention = convention if convention
      name = consume_identifier
      signature = parse_signature(peek.location, inherited_type_params, receiver)
      indent = loc.column == 0 ? 0_u32 : loc.column - 1
      if peek.is_a?(Token::Colon)
        block = parse_colon_and_block(indent)
        consume?(Token::Newline) || report_error(peek.location, "Expected newline after method body")
      end
      Ast::AbstractMethod.new(loc, name, signature, block)
    end
  end

  def parse_constructor(loc : Location, return_type : Ast::Type, type_params : Array(Ast::TypeParameter)? = nil)
    @log.debug_descend(loc, "Parsing constructor...") do
      signature = parse_signature(loc, type_params)
      if t = signature.return_type
        report_error(t.location, "Constructor cannot have a return type")
      end
      signature.return_type = return_type
      indent = loc.column == 0 ? 0_u32 : loc.column - 1
      block = parse_colon_and_block(indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after constructor body")
      Ast::Function.new(loc, return_type.name, signature, block)
    end
  end

  def parse_static(loc : Location, type_name : String, inherited_type_params : Array(Ast::TypeParameter)? = nil)
    @log.debug_descend(loc, "Parsing static method...") do
      name = consume_identifier
      signature = parse_signature(peek.location, inherited_type_params)
      indent = loc.column == 0 ? 0_u32 : loc.column - 1
      block = parse_colon_and_block(indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after static method body")
      Ast::Function.new(loc, "#{type_name}.#{name}", signature, block)
    end
  end

  def parse_trait(decl_loc : Location) : Ast::Trait?
    @log.debug_descend(decl_loc, "Parsing trait declaration...") do
      convention, name, type_params, traits = parse_type_header
      trait_dec = Ast::Trait.new(decl_loc, convention, name, type_params, traits)
      receiver = Ast::Parameter.new(decl_loc, "self", Ast::Type.new(decl_loc, "Self"))
      until eof?
        if peek.location.column <= decl_loc.column
          break
        end
        key_tok = consume!(Token::KeyWord)
        case key_tok.data
        when KeyWord::Field
          report_error(key_tok.location, "Fields not yet supported by traits.")
          parse_field(key_tok.location)
        when KeyWord::Def
          trait_dec.methods << parse_abstract_method(key_tok.location, receiver, type_params)
        when KeyWord::Constructor
          report_error(key_tok.location, "Constructors not yet supported by traits.")
          parse_constructor(key_tok.location, receiver.type, type_params)
        when KeyWord::Static
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
        if peek.location.column <= decl_loc.column
          break
        end
        if variant_token = consume?(Token::Type)
          enum_dec.variants << parse_variant(variant_token.location, variant_token.data.as(String))
          next
        end
        key_tok = consume!(Token::KeyWord)
        case key_tok.data
        when KeyWord::Field
          # enum_dec.fields << parse_field(key_tok.location)
          report_error(key_tok.location, "Enums cannot have fields directly, fields should be in variants.")
          parse_field(key_tok.location)
        when KeyWord::Def
          @declarations << parse_method(key_tok.location, receiver, type_params)
        when KeyWord::Constructor
          @declarations << parse_constructor(key_tok.location, receiver.type, type_params)
        when KeyWord::Static
          @declarations << parse_static(key_tok.location, name, type_params)
        else
          report_error(peek.location, "Unexpected token in struct declaration")
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
      if consume?(Token::RParen)
        @log.warning(loc, "Variant #{name} has no fields.")
        return Ast::Variant.new(loc, name, [] of {String, Ast::Type})
      end
      with_context ParserContext::List do
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
          consume?(Token::Comma) || break
        end
        consume!(Token::RParen)
        Ast::Variant.new(loc, name, fields)
      end
    end
  end

  def parse_import(decl_loc : Location) : Ast::Node? # TODO: Define Ast::Import
    @log.debug(decl_loc, "Parsing import declaration...")
    # Pony: parse_import(loc)
    raise report_error(decl_loc, "parse_import not yet implemented")
    nil # Placeholder
  end

  def parse_extend(decl_loc : Location) : Ast::Extend?
    @log.debug_descend(decl_loc, "Parsing extend declaration...") do
      type_params = parse_type_parameters?
      type = parse_type_expression
      traits = parse_traits?
      extension = Ast::Extend.new(decl_loc, type_params, type, traits)
      @declarations << extension
      receiver = Ast::Parameter.new(decl_loc, "self", type)
      until eof?
        if peek.location.column <= decl_loc.column
          break
        end
        key_tok = consume!(Token::KeyWord)
        case key_tok.data
        when KeyWord::Field
          report_error(key_tok.location, "Type extensions cannot add fields, only methods.")
          parse_field(key_tok.location)
        when KeyWord::Def
          @declarations << parse_method(key_tok.location, receiver, type_params)
        when KeyWord::Constructor
          @declarations << parse_constructor(key_tok.location, type, type_params)
        when KeyWord::Static
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
  def next_token
    parser.next_token
  end
  # def parse_expression(block_indent : UInt32, stop_at : StopAt = StopAt::Normal)
  #   parser.parse_expression(block_indent, stop_at)
  # end
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
  property line_indent : UInt32
  property stop : StopAt
  property expecting : Expecting = Expecting::Term
  
  def initialize(
    @parser : Parser, 
    @expression_indent : UInt32, 
    @line_indent : UInt32,
    @stop : StopAt)
  end
  def initialize(@parser : Parser, @stop : StopAt = StopAt::Normal)
    @expression_indent = peek.location.indent
    @line_indent = @expression_indent
  end
  def initialize(@parser : Parser, @expression_indent : UInt32, @stop : StopAt = StopAt::Normal)
    @line_indent = @expression_indent
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
      # // terms.push(match pop_term()?
      # // | let i: IntLiteral then IntLiteral(i.loc, i.value * -1)
      # // | let f: FloatLiteral then FloatLiteral(f.loc, f.value * -1.0)
      # // | let n: Node then NegNode(n)
      # // end)
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
    when Token::RParen, Token::RBracket, Token::RBrace, Token::Colon, Token::EOF
      true
    when Token::Newline
      tok.data <= expression_indent
    else
      case {stop, tok}
      when {StopAt::BooleanOperator, Token::Operator}
        tok.data.compares?
      when {StopAt::ColonOrAnd, Token::Colon}
        true
      when {StopAt::ColonOrAnd, Token::Operator}
        tok.data == Operator::And
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
          # terms << function_call(tok.location)
          # Expecting::Operator
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
    # when KeyWord::Let then handle_arg_passing_mode(loc, "let")
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

  # def handle_arg_passing_mode(loc : Location, word : String)
    # case tok = peek
    # when Token::Type, Token::Variable
    #   node = Ast::Identifier.new(tok.location, next_token.data.as(String))
    #   if tok.is_a?(Token::Dot)
    #     next_token
    #     case tok = peek
    #     when Token::Type, Token::Variable
    #       # function_call(tok.location, tok.data, [Mode.from_string(word)], [node])
    #       call = function_call(node.location, node.name)
          
    #     else 
    #       raise report_error(peek.location, "Expected function name after dot.")
    #     end
    #   else
    #     raise report_error(loc, "Illegal use of argument passing mode.  ")
    #   end
    # else
    #   log.warning(loc, "Using argument passing mode as an identifier.")
    #   Ast::Identifier.new(loc, word)
    # end
  # end
  
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
      parser.with_context ParserContext::List do
        return Ast::EmptyArray.new(loc) if consume?(Token::RBracket)
        statements = parse_list
        consume?(Token::RBracket) || raise report_error(peek.location, "Expected closing bracket.")
        if statements.size == 0
          log.warning(loc, "empty array literal")
          Ast::EmptyArray.new(loc)
        else
          log.debug(loc, "array literal with #{statements.size} elements")
          Ast::Array.new(loc, statements)
        end
      end
    end
  end
  
  # number parsing has been moved to the lexer
  # def parse_number_literal(loc : Location, value : String)
  #   log.debug_descend(loc, "parse_number_literal") do
  #     try 
  #       Ast::Int.new(loc, value.isize()?)
  #     else
  #       try 
  #         Ast::Float.new(loc, value.f64()?)
  #       else
  #         raise report_error(loc, "unrecognized number literal: " + value)
  #         error
  #       end
  #     end
  
  # handle a left paren or left bracket when encountered in operator position
  # returns a Call node
  # def function_call(loc : Location, fn_name : String? = nil) : Ast::Call
  #   log.debug_descend(
  #       loc, 
  #       "function_call; top-term-of-stack: #{begin last_term! rescue "<none>" end};  next: #{peek()}") do
  #     fn_name ||= case node = pop_term!
  #     when Ast::Identifier then node.name
  #     when Ast::Type
  #       if node.type_args
  #         report_error(node.location, "Type with type arguments cannot be used as a function name.")
  #       end
  #       node.name
  #     when Ast::MethodCall
  #       node.name
  #     else 
  #       raise report_error(node.location, "Expected function name; got #{node}")
  #     end
  #     type_args = parser.parse_type_args?
  #     args = parse_args
  #     Ast::Call.new(loc, fn_name, type_args, args)
  #   end
  # end
  
  # def handle_dot()
  #   log.debug_descend(
  #       peek.location, 
  #       "handle_dot; top-term-of-stack: #{last_term { "<none>" }};  next: #{peek()}") do
  #     node = pop_term!
  #     name = consume_identifier
  #     Ast::MethodCall.new(node, name)
  #     # node = Ast::MethodCall.new(node, name)
  #     # case peek()
  #     # when Token::LBracket, Token::LParen
  #     #   node.call = function_call(node.location, name)
  #     # else
  #     # end
  #     # node
  #   end
  # end

  def handle_type(loc : Location, type : String)
    if consume?(Token::Dot)
      log.debug(loc, "static method")
      name = consume_identifier
      Ast::StaticCall.new(loc, type, function_call(loc, name))
    else
      function_call(loc, type)
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
      list = parse_list(args)
      consume!(Token::RParen)
      list
    end
  end

  def parse_list(exprs : Array(Ast::Expr) = [] of Ast::Expr) : Array(Ast::Expr)
    parser.with_context ParserContext::List do
      until expression_done?
        # convention = parser.read_convention?
        exprs << ExpressionParser.new(parser, expression_indent, StopAt::Comma).parse
        next if consume?(Token::Comma)
        # break if expression_done?
        raise report_error(peek.location, "Expected comma or closing bracket/paren; got #{peek} in list!")
        # case tok = peek
        # when Token::Comma
        #   next_token
        # when Token::RParen, Token::RBracket, Token::RBrace
        #   break
        # else
        #   raise report_error(tok.location, "should not encounter #{tok} in list")
        # end
      end
      log.debug(peek.location, "done parsing list: #{exprs}")
      exprs
    end
  end
  
  # def parse_let_declaration(loc : Location)
  #   tok = peek
  #   name = consume_identifier
  #   if peek.is_a?(Token::Dot)
  #     rewind()
  #     handle_arg_passing_mode(loc, "let")
  #   else
  #     consume!(Token::Assign)
  #     value = parse_expression(block_indent, StopAt::ExpressionEnd)
  #     Ast::Let.new(loc, name, value)
  #   end
  # end
  
  def parse_var_declaration(loc : Location)
    name = consume_identifier
    consume!(Operator::Assign)
    value = ExpressionParser.new(parser, expression_indent, line_indent, stop).parse
    Ast::Var.new(loc, name, value)
  end
    
  def parse_const_declaration(loc : Location)
    name = consume_identifier
    consume!(Operator::Assign)
    value = ExpressionParser.new(parser, expression_indent, line_indent, stop).parse
    Ast::Const.new(loc, name, value)
  end
    
  def parse_if_expression(loc : Location)
    # UcsParser.new(parser).parse(loc, block_indent)
    raise report_error(loc, "if expressions not yet implemented")
  end
  
  def parse_while_expression(loc : Location)
    condition = ExpressionParser.new(parser, expression_indent, line_indent, StopAt::ColonOrAnd).parse
    body = parser.parse_colon_and_block(line_indent)
    Ast::WhileLoop.new(loc, condition, body)
  end
  
  def parse_for_expression(loc : Location)
    var_loc = peek.location
    var_name = consume!(Token::Variable).data.as(String)
    unless consume?(Operator::In)
      report_error(peek.location, "Expected \"in\" after variable name in for loop; got #{peek}")
    end
    iterator = ExpressionParser.new(parser, expression_indent, line_indent, StopAt::ColonOrAnd).parse
    body = parser.parse_colon_and_block(line_indent)
    Ast::ForLoop.new(loc, Ast::Identifier.new(var_loc, var_name), iterator, body)
  end
end

class Parser
  property lexer : Lexer
  @peeked : Token? = nil
  property context : ParserContext = ParserContext::TopLevel
  property indent : UInt32 = 0  # indentation of the first token of the current expression, or if, struct, trait, def, etc
  # ^ or maybe this should be passed by methods on the stack?  since it's a little stacky
  property log : Logger
  getter declarations = [] of Ast::TopLevelItem # The final list of top-level AST nodes

  # --- Exception for Parse Errors to aid in recovery ---
  class ParseError < Exception
    getter location : Location
    def initialize(message : String, @location : Location)
      super("#{@location}: #{message}")
    end
  end

  def initialize(@lexer : Lexer, @log : Logger)
  end
  def initialize(text : String, log_level : Logger::Level = Logger::Level::Warning)
    @log = Logger.new(log_level)
    @lexer = Lexer.new(Lexer::Reader.new(text), @log)
  end

  # == Main Entry Point ==
  def parse : Array(Ast::TopLevelItem)
    @log.info(Location.zero, "Parser.parse start")
    consume? Token::BeginFile

    until eof?
      begin
        # parse_top_level_item is expected to add items to @declarations directly
        # if it parses a construct that can produce multiple items (like 'function' block).
        # Otherwise, it returns a single item.
        if item = parse_top_level_item
          @declarations << item
        end
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
  #   loc = peek.location
  #   @log.debug(loc, "Parsing expression...")
  #   report_error(loc, "parse_expression not yet implemented")
  #   tokens_consumed = String.build do |str|
  #     while true
  #       if peek.is_a?(Token::Newline)
  #         break
  #       else
  #         str << ' ' << next_token.short
  #       end
  #     end
  #   end
  #   @log.debug(loc, "Consumed expression:#{tokens_consumed}")
  #   # consume_until(Token::Newline)
  #   Ast::Nil.new(loc)
  # end

  # TODO: Implement ExpressionParser class (for `parse_expression`)
  # TODO: Implement UcsParser class (for `parse_if_expression`)
end