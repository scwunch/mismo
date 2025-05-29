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

class Parser
  property lexer : Lexer
  @peeked : Token? = nil
  property context : ParserContext = ParserContext::TopLevel
  property indent : UInt32 = 0  # indentation of the first token of the current expression, or if, struct, trait, def, etc
  # ^ or maybe this should be passed by methods on the stack?  since it's a little stacky
  @log : Logger
  getter declarations = [] of Ast::TopLevelItem # The final list of top-level AST nodes

  # --- Exception for Parse Errors to aid in recovery ---
  class ParseError < Exception
    getter location : Location
    def initialize(message : String, @location : Location)
      super(message)
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
      # Skip any leading newlines or comments between top-level items
      skipped_newlines_or_comments = false
      while peek.is_a?(Token::Newline) || peek.is_a?(Token::Comment)
        next_token
        skipped_newlines_or_comments = true
        break if eof?
      end
      break if eof?

      begin
        # parse_top_level_item is expected to add items to @declarations directly
        # if it parses a construct that can produce multiple items (like 'function' block).
        # Otherwise, it returns a single item.
        item = parse_top_level_item
        if item # Add if it's a single item
          @declarations << item
        elsif !skipped_newlines_or_comments && !eof? && !peek.is_a?(Token::KeyWord)
          # If parse_top_level_item returned nil, and we haven't just skipped newlines,
          # and we're not at a keyword, it implies an error or unexpected token.
          # This case might be hit if parse_top_level_item handles adding to declarations itself (like function blocks)
          # and successfully parses but returns nil to signal it.
          # However, if it returns nil due to an *unhandled* token at the start of parse_top_level_item,
          # we need to advance to avoid an infinite loop.
          # The current parse_top_level_item already reports an error for non-keywords.
        end
      rescue err : ParseError
        # Logged by report_error, here we just ensure we can continue to next top-level item
        @log.debug(err.location, "Caught ParseError: #{err.message}, attempting to find next top-level item.")
        # Basic recovery: try to find a sensible place to restart, e.g., after next significant newline or known keyword
        recover_to_next_declaration
      end
    end

    @log.info(peek.location, "Parser.parse end, #{@declarations.size} declarations found.")
    @declarations
  end

  # --- Token Navigation & Consumption ---

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


  # --- Error Reporting & Recovery ---
  def report_error(location : Location, message : String) : ParseError
    @log.error(location, message)
    ParseError.new(message, location)
  end

  def recover_to_next_declaration
    # Simple recovery: skip tokens until we find a keyword that typically starts a new declaration,
    # or after a significant newline sequence. This can be made more sophisticated.
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
      # Could also look for patterns like multiple newlines, or end-of-block tokens if not nested.
      next_token # Consume and discard the token
    end
    @log.debug(peek.location, "Recovery reached EOF or skipped too many tokens.")
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

  def consume_variable_name(ignore_newline = false) : String
    # case token = ignore_newline ? next_token(:skip_newline) : next_token
    case token = next_token
    when Token::Variable
      token.data
    when Token::Type # Check if it's a mis-cased variable name
      report_error(token.location, "Variable names usually start with lowercase; got '#{token.data}' (a type name pattern).")
      token.data
    else
      raise report_error(token.location, "Expected variable name, got #{token}")
    end
  end

  # --- Top-Level Parsing ---
  def parse_top_level_item : Ast::TopLevelItem?
    loc = peek.location
    @log.debug(loc, "Parsing top-level item, current token: #{peek}")

    # newlines and comments between items are skipped by the main parse loop
    return nil if eof?
    
    # Based on the Pony parser's `parse_top_level_statement`
    # The Pony version consumes the keyword *before* calling the specific parse method.
    # We'll do the same: `consume` the keyword token, then dispatch.
    
    keyword_token = peek
    unless keyword_token.is_a?(Token::KeyWord)
      report_error(keyword_token.location, "Expected top-level declaration (struct, function, enum, etc.); got #{keyword_token}")
      consume_until([Token::Newline.class]) # Try to skip to next line
      return nil
    end

    # Now we know it's a KeyWord token, get its data
    keyword_data = keyword_token.data.as(KeyWord)
    next_token # Consume the keyword token itself

    case keyword_data
    when KeyWord::Function
      # parse_function_block will add Ast::Function nodes directly to @declarations
      parse_function_block(loc)
      nil # Signal that items were added directly 
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
    # Add other top-level keywords here (e.g., const, global var if language supports)
    else
      report_error(loc, "Unexpected keyword for top-level declaration: '#{keyword_data}'")
      # Attempt to recover by consuming until a known top-level keyword or EOF
      consume_until([Token::KeyWord.class, Token::EOF.class])
      rewind if peek.is_a?(Token::KeyWord) # Rewind to let the next iteration try the keyword
      nil
    end
  end

  # --- Function Parsing ---
  # Handles `function NAME ... def ... end`
  # Adds Ast::Function nodes directly to @declarations.
  def parse_function_block(function_keyword_loc : Location)
    block_name = consume_identifier("function block name")
    return if block_name.nil?  # Error already reported by consume_identifier

    @log.info(function_keyword_loc, "Parsing function block '#{block_name}'")
    @log.debug_descend

    type_params = parse_type_parameters?

    # check if this is a single function or a function block
    case peek
    when Token::Def
      # continue
    when Token::LBrace, Token::Type, Token::Colon
      parse_def_overload(function_keyword_loc, block_name, type_params)
      return
    else
      raise report_error(peek.location, "Expected 'def' or function signature after function block name")
    end

    until eof?
      break unless def_tok = consume?("def")
      
      if overload_ast = parse_def_overload(def_tok.location, block_name, type_params)
        @declarations << overload_ast
      else
        # Error in parsing overload, already reported. Attempt to recover to next 'def' or end of block.
        recover_to_next_def_or_end_of_function_block
      end
    end

    
    @log.info(peek.location, "Finished parsing function block '#{block_name}'")
  end

  # Parses a single `def [TYPE_PARAMS]? (PARAMS)? RETURN_TYPE : BODY`
  # within a function block
  def parse_def_overload(def_loc : Location, name : String, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function?
    @log.debug_descend(def_loc, "Parsing def for '#{name}'") do
      sig = parse_signature(peek.location, inherited_type_params)
      return nil unless sig
      
      with_context(ParserContext::Block) do
        body = parse_colon_and_block(def_loc.column)
        consume?(Token::Newline) || report_error(peek.location, "Expected newline after function body")
        return nil unless body
      end
    end
    Ast::Function.new(loc: def_loc, name: name, signature: sig, body: body)
  end

  # Parses a `def CONVENTION? NAME [TYPE_PARAMS]? (PARAMS)? RETURN_TYPE : BODY`
  # within a struct or enum block
  # The syntax sugar `def .method` is parsed as `def method(self SelfType)`
  # and `def mut.method(mut self SelfType)` is parsed as `def method(mut self SelfType)`
  def parse_def_method(name : String?, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function?
    def_loc = peek.location
    parse_def_overload(def_loc, name, inherited_type_params)
  end

  def parse_abstract_def_method(name : String?, inherited_type_params : Array(Ast::TypeParameter)?) : Ast::Function?
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
          constraints = parse_constraints?
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

  def parse_list(delimiter : (Token::Comma.class | Operator::And.class), &block)
    peek # to skip the possible leading newline before entering into list context
    until eof?
      yield block
      consume?(delimiter) || break
    end
    consume!(delimiter)
  end

  # Parses `: Trait & Trait[Int]`
  def parse_constraints?
    # consume optional colon or `is`
    return nil unless consume?(Token::Colon) || consume?(Operator::Is) || peek.is_a?(Token::Type)
    @log.debug_descend(peek.location, "Parsing constraints...") do
      # contraint parsing may occur in a list context, but we still want to require
      # the `&` separator yet ignore newlines, so we use ParserContext::TopLevel
      with_context ParserContext::TopLevel do
        constraints = [] of Ast::Type
        until eof?
          constraints << parse_type_expression
          consume?(Operator::And) || break
        end
        constraints
      end
    end
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

  # Reads an optional convention keyword (let, mut, sink, copy, ref)
  def read_convention? : Convention # Convention = Mode | Nil
    tok = peek
    if tok.is_a?(Token::KeyWord)
      case tok.data
      when KeyWord::Let   then next_token; Mode::Let
      # TODO: Add other keywords that map to Mode enum (mut, sink, copy, ref)
      # when KeyWord::Mut   then next_token; Mode::Mut
      # when KeyWord::Sink  then next_token; Mode::Sink
      # when KeyWord::Copy  then next_token; Mode::Copy
      # when KeyWord::Ref   then next_token; Mode::Ref
      else 
        nil  # Not a convention keyword
      end
    # Pony also allowed string matches for "mut", "sink", etc.
    # If these are lexed as VariableName tokens but act as conventions:
    elsif tok.is_a?(Token::Variable)
      case tok.data
      when "let" then next_token; Mode::Let
      when "mut"  then next_token; Mode::Mut
      when "sink" then next_token; Mode::Sink
      when "ref"  then next_token; Mode::Ref
      when "copy" then next_token; Mode::Copy
      else
        nil # Not a convention keyword
      end
    else
      nil # No convention keyword found
    end
  end

  # Parses a type expression, e.g., MyType, MyGenericType[Arg1, Arg2]
  def parse_type_expression : Ast::Type
    loc = peek.location
    name = consume_type_name!
    @log.debug_descend(loc, "Parsing type expression: #{name}") do
      type_args = parse_type_args?
      Ast::Type.new(loc: loc, name: name, type_args: type_args)
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
      parse_statements(block_base_indent)
    end
  end
  
  # Parses a sequence of statements until indent decreases or block ends
  def parse_statements(current_block_indent : UInt32, stop_at : StopAt = StopAt::Normal) : Array(Ast::Expr)
    # TODO: This is a critical part that will involve the ExpressionParser.
    # For now, placeholder.
    @log.debug_descend(peek.location, "parse_statements (indent: #{current_block_indent}, stop: #{stop_at}) - NOT FULLY IMPLEMENTED") do
      with_context ParserContext::Block do
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
            statements << parse_expression
          end
        end
        statements
      end
    end
  end

  def recover_to_next_def_or_end_of_function_block
    @log.debug(peek.location, "Attempting recovery within function block...")
    count = 0
    until eof? || count > 200 # Safety break
      count += 1
      tok = peek
      # Look for 'def' or a token that clearly indicates end of the 'function NAME' block
      if tok.is_a?(Token::KeyWord) && tok.data == KeyWord::Def
        @log.debug(tok.location, "Recovery found next 'def' keyword.")
        return
      end
      # Heuristic: if we encounter another top-level keyword, the 'function' block was likely malformed and ended.
      if tok.is_a?(Token::KeyWord)
        case tok.data
        when KeyWord::Struct, KeyWord::Enum, KeyWord::Trait, KeyWord::Extend, KeyWord::Function, KeyWord::Import
          @log.debug(tok.location, "Recovery found another top-level keyword '#{tok.data}', assuming end of current function block.")
          rewind # Put the keyword back for the main loop
          return
        end
      end
      next_token
    end
    @log.debug(peek.location, "Recovery in function block reached EOF or limit.")
  end

  def parse_type_header : {String, Array(Ast::TypeParameter)?, Array(Ast::Type)?}
    name = consume_identifier
    type_params = parse_type_parameters?
    traits = parse_constraints?
    {name, type_params, traits}
  end
    

  # parses a struct, including traits, fields, methods, and any other sub-items
  # ```
  # struct Point is Stringable
  #   field x Int
  #   field y Int
  # ```
  def parse_struct(decl_loc : Location) : Ast::Struct?
    @log.debug_descend(decl_loc, "Parsing struct declaration...") do
      name, type_params, traits = parse_type_header
      struct_dec = Ast::Struct.new(decl_loc, name, type_params, traits)
      receiver = Ast::Parameter.new(decl_loc, "self", Ast::Type.new(decl_loc, name))
      until eof?
        if peek.location.column <= decl_loc.column
          break
        end
        key_tok = consume!(Token::KeyWord)
        case key_tok.data
        when KeyWord::Field
          struct_dec.fields << parse_field(key_tok.location)
        when KeyWord::Def
          declarations << parse_method(key_tok.location, receiver, type_params)
        when KeyWord::Constructor
          declarations << parse_constructor(key_tok.location, name, type_params)
        when KeyWord::Static
          declarations << parse_static(key_tok.location, name, type_params)
        else
          report_error(peek.location, "Unexpected token in struct declaration")
          break
        end
      end
      declarations << struct_dec
      struct_dec
    end
  end

  def parse_field(loc : Location) : Ast::Field
    @log.debug_descend(loc, "Parsing field...") do
      name = consume_identifier
      # optional colon
      consume?(Token::Colon)
      type = parse_type_expression
      init = consume?(Operator::Assign) && parse_expression
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

  def parse_constructor(loc : Location, type_name : String, inherited_type_params : Array(Ast::TypeParameter)? = nil)
    @log.debug_descend(loc, "Parsing constructor...") do
      signature = parse_signature(loc, inherited_type_params)
      if t = signature.return_type
        report_error(t.loc, "Constructor cannot have a return type")
      end
      signature.return_type = Ast::Type.new(loc, type_name, Ast.to_type_args(inherited_type_params))
      indent = loc.column == 0 ? 0_u32 : loc.column - 1
      block = parse_colon_and_block(indent)
      consume?(Token::Newline) || report_error(peek.location, "Expected newline after constructor body")
      Ast::Function.new(loc, type_name, signature, block)
    end
  end

  def parse_static(loc : Location, type_name : String, inherited_type_params : Array(Ast::TypeParameter)? = nil)
    raise report_error(loc, "parse_static is not yet implemented")
    Ast::Function.new(loc, "#{type_name}.#{consume_identifier}", Ast::Signature.new(loc), [] of Ast::Expr)
  end

  def parse_trait(decl_loc : Location) : Ast::Trait?
    @log.debug(decl_loc, "Parsing trait declaration...")
    # Pony: parse_trait() -> parse_type_header(), then methods
    raise report_error(decl_loc, "parse_trait not yet implemented")
    nil # Placeholder
  end

  def parse_enum(decl_loc : Location) : Ast::Enum?
    @log.debug(decl_loc, "Parsing enum declaration...")
    # Pony: parse_enum() -> parse_type_header(), parse_variants(), then methods
    raise report_error(decl_loc, "parse_enum not yet implemented")
    nil # Placeholder
  end

  def parse_import(decl_loc : Location) : Ast::Node? # TODO: Define Ast::Import
    @log.debug(decl_loc, "Parsing import declaration...")
    # Pony: parse_import(loc)
    raise report_error(decl_loc, "parse_import not yet implemented")
    nil # Placeholder
  end

  def parse_extend(decl_loc : Location) : Ast::Extend?
    @log.debug(decl_loc, "Parsing extend declaration...")
    # Pony: parse_extend(loc.col)
    raise report_error(decl_loc, "parse_extend not yet implemented")
    nil # Placeholder
  end

  def parse_expression
    loc = peek.location
    @log.debug(loc, "Parsing expression...")
    report_error(loc, "parse_expression not yet implemented")
    tokens_consumed = String.build do |str|
      while true
        if peek.is_a?(Token::Newline)
          break
        else
          str << ' ' << next_token.short
        end
      end
    end
    @log.debug(loc, "Consumed expression:#{tokens_consumed}")
    # consume_until(Token::Newline)
    Ast::Nil.new(loc)
  end

  # TODO: Implement parse_signature, parse_parameters, parse_type_expression,
  # parse_colon_and_block, parse_type_parameters, parse_constraints, etc.

  # TODO: Implement ExpressionParser class (for `parse_expression`)
  # TODO: Implement UcsParser class (for `parse_if_expression`)

end
