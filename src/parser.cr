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
  @tokens : Array(Token)
  @index : UInt32 = 0
  @log : Logger
  getter declarations = [] of Ast::TopLevelItem # The final list of top-level AST nodes

  # --- Exception for Parse Errors to aid in recovery ---
  class ParseError < Exception
    getter location : Location
    def initialize(message : String, @location : Location)
      super(message)
    end
  end

  def initialize(@tokens : Array(Token), @log : Logger)
  end

  # == Main Entry Point ==
  def parse : Array(Ast::TopLevelItem)
    @log.info(Location.zero, "Parser.parse start")
    consume_token_if { |tok| tok.is_a?(Token::BeginFile) }

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

  # Peeks at the token at the current index + offset without consuming.
  # Returns Token::EOF if out of bounds.
  def peek(offset : Int) : Token
    effective_index = @index + offset
    if effective_index < 0
      # This case should ideally be handled by callers ensuring offset is valid
      # or by having a specific BeginFile token at the start if needed.
      # For now, returning a synthetic BeginFile token.
      Token::BeginFile.new(Location.zero)
    elsif effective_index < @tokens.size
      @tokens[effective_index]
    else
      # Return an EOF token with the location of the last actual token or start if empty
      last_loc = @tokens.empty? ? Location.zero : @tokens.last.location
      Token::EOF.new(last_loc)
    end
  end

  # Peeks at the current token without consuming
  def peek : Token
    @tokens[@index] rescue peek(offset: 0)
  end

  # Peeks at the current token without consuming, or peeks at the 
  # next token if the current one is a newline.
  macro peek(skip_newline)
    {% if skip_newline.is_a?(Int)
      raise "Invalid call, try peek(offset: #{skip_newline})"
    elsif skip_newline != :skip_newline
      raise "Invalid option: #{skip_newline}; must be :skip_newline, or int" 
    end %}
    if (tok = peek).is_a?(Token::Newline)
      peek(offset: 1)
    else
      tok
    end
  end

  # Consumes the current token and returns it. Advances the index.
  # Returns the last token if already at EOF to prevent errors on multiple calls.
  def next_token : Token
    peek
  ensure
    @index += 1 unless eof?
  end

  # Consumes the current token and returns it. Advances the index.
  # If the current token is a newline, it consumes the next token instead.
  macro next_token(skip_newline)
    {% unless skip_newline == :skip_newline 
      raise "Invalid option: #{skip_newline}; must be :skip_newline" 
    end %}
    if (tok = peek).is_a?(Token::Newline)
      @index += 1
      next_token
    else
      @index += 1 unless eof?
      tok
    end  
  end

  # Returns the previously consumed token.
  def prev_token : Token
    peek(-1)
  end

  # Moves the current token index back by `count` positions.
  def rewind(count : Int = 1)
    @index -= count
    @index = 0 if @index < 0
  end

  # Checks if the parser has reached the end of the token stream.
  def eof?(offset : Int = 0) : Bool
    @index + offset >= @tokens.size
  end

  # Consumes the current token if the block yields true for it.
  # Returns the token if consumed, nil otherwise.
  def consume?(ignore_newline : Bool = false, &block : Token -> Bool) : Token?
    if ignore_newline && yield peek(:skip_newline) || yield peek
      next_token
    else
      nil
    end
  end

  def consume?(&block : Token -> Bool) : Token?
    if yield peek
      next_token
    else
      nil
    end
  end

  # macro consume?(skip_newline)

  # Consumes the current token if it's of the expected kind.
  # If not, reports an error and returns nil.
  def consume?(expected_kind : Token.class) : Token?
    if (token = peek).class == expected_kind
      next_token
      token
    end
  end

  def consume?(expected_kind : T.class, skip_newline) : T? forall T
    unless skip_newline == :skip_newline 
      raise "Invalid option: #{skip_newline}; must be :skip_newline" 
    end
    if (token = peek(:skip_newline)).is_a?(T)
      next_token(:skip_newline)
      token
    end
  end

  # # Overload for specific keyword data
  # def consume?(expected_keyword : KeyWord) : Token?
  #   token = peek
  #   if token.is_a?(Token::KeyWord) && token.data == expected_keyword
  #     next_token
  #   else
  #     report_error(token.location, "Expected keyword '#{expected_keyword}', got #{token}")
  #     nil
  #   end
  # end
  
  # # Overload for specific operator data
  # def consume?(expected_operator : Operator) : Token?
  #   if (token = peek).is_a?(Token::Operator) && token.data == expected_operator
  #     next_token
  #   end
  # end

  # Overload for specific token data
  def consume?(expected_data : KeyWord | Operator | String) : Token?
    if peek.data == expected_data
      next_token
    end
  end

  def consume?(expected_data : KeyWord | Operator | String, skip_newline) : Token?
    unless skip_newline == :skip_newline 
      raise "Invalid option: #{skip_newline}; must be :skip_newline" 
    end
    if (token = peek(:skip_newline)).data == expected_data
      next_token(:skip_newline)
    end
  end

  def consume!(expected)
    consume?(expected) || raise report_error(peek.location, "Expected #{expected}, got #{peek}")
  end

  macro consume!(expected, skip_newline)
    {% unless skip_newline == :skip_newline 
      raise "Invalid option: #{skip_newline}; must be :skip_newline" 
    end %}
    consume?({{expected}}, :skip_newline) || raise report_error(peek.location, "Expected #{{{expected}}}, got #{peek(:skip_newline)}")
  end
    

  # Consumes tokens until one of the `target_kinds` is encountered or EOF.
  # Returns the token that matched or the EOF token.
  def consume_until(target_kinds : Array(Token.class)) : Token
    skipped_tokens = [] of Token
    while !eof?
      current = peek
      return current if target_kinds.any? { |kind| current.class == kind }
      skipped_tokens << next_token
    end
    @log.debug(peek.location, "Consumed until EOF. Skipped: #{skipped_tokens.join(", ")}") unless skipped_tokens.empty?
    peek # EOF
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
  def consume_identifier(context_message : String = "identifier", ignore_newline : Bool = false) : String
    case token = ignore_newline ? next_token(:skip_newline) : next_token
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

  def consume_type_name!(ignore_newline = false) : String
    case token = ignore_newline ? next_token(:skip_newline) : next_token
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
    if ignore_newline
      if peek(:skip_newline).is_a?(Token::Type)
        next_token(:skip_newline).as(Token::Type).data
      end
    else
      if (tok = peek).is_a?(Token::Type)
        next_token.as(Token::Type).data
      end
    end
  end

  def consume_variable_name(ignore_newline = false) : String
    case token = ignore_newline ? next_token(:skip_newline) : next_token
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
    next_token(ignore_newline: true) # Consume the keyword token itself

    case keyword_data
    when KeyWord::Function
      # parse_function_block will add Ast::Function nodes directly to @declarations
      parse_function_block(loc)
      nil # Signal that items were added directly 
    when KeyWord::Struct
      parse_struct_declaration(loc)
    when KeyWord::Trait
      parse_trait_declaration(loc)
    when KeyWord::Enum
      parse_enum_declaration(loc)
    when KeyWord::Import
      raise "This should have been handled in the lexer"
    when KeyWord::Extend
      parse_extend_declaration(loc)
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

    while true
      # Skip newlines/comments before the next `def` or end of block
      if peek.is_a?(Token::Newline)
        next_token(ignore_newline: true)
      end
      break if eof?

      # Consume 'def' keyword or break
      def_loc = (consume_token_if { |tok| tok.data == KeyWord::Def } || break).location

      if overload_ast = parse_def(def_loc, block_name, type_params)
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

      body = parse_colon_and_block(def_loc.column)
      return nil unless body
      
      consume?(Token::Newline)
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

  def parse_signature(sig_loc : Location, inherited_type_params : Array(Ast::TypeParameter)? = nil) : Ast::Signature?
    @log.debug_descend(sig_loc, "Parsing signature...") do
      type_params = parse_type_parameters?(inherited_type_params)
      params = parse_parameters?
      return_type = 
        if peek(:skip_newline).is_a?(Token::Type)
          parse_type_expression
        else
          nil
        end
      Ast::Signature.new(sig_loc, type_params, params, return_type)
    end
  end

  # Parses `[T, U: Constraint]` or `[A
  def parse_type_parameters?(inherited_type_params : Array(Ast::TypeParameter)? = nil) : Array(Ast::TypeParameter)?
    type_params = inherited_type_params.try &.dup
    return type_params unless peek.is_a?(Token::LBracket)
    
    type_params ||= [] of Ast::TypeParameter
    lbracket_loc = next_token.location  # consume '['
    
    @log.debug_descend(lbracket_loc, "Parsing type parameters...") do
      while true
        return type_params if consume?(Token::RBracket, :skip_newline)
        # parse type parameter
        param_loc = peek.location
        name = consume_type_name!
        constraints = parse_constraints?
        type_params << Ast::TypeParameter.new(param_loc, name, constraints)
        consume?(Token::Comma, :skip_newline)
        consume?(Token::Newline)
      end
    end
  end

  # Parses `: Trait & Trait[Int]`
  def parse_constraints?
    return nil unless consume?(Token::Colon, :skip_newline) || peek(:skip_newline).is_a?(Token::Type)
    @log.debug_descend(peek.location, "Parsing constraints...") do
      constraints = [] of Ast::Type
      until eof?
        constraints << parse_type_expression
        consume?(Operator::And, :skip_newline) || break
      end
      constraints
    end
  end

  macro parse_parameters(receiver)
    {% unless receiver.is_a?(ArrayLiteral) && receiver.size <= 1 %}
      {% raise "Receiver must be an array-literal of zero or one parameters." %}
    {% end %}
    params : Array(Ast::Parameter) = {{receiver}}
    return params unless consume?(Token::LParen, :skip_newline)

    @log.debug_descend(peek.location, "Parsing parameters...") do
      while true
        return params if consume?(Token::RParen, :skip_newline)
        params << parse_parameter
        consume?(Token::Comma, :skip_newline)
        consume?(Token::Newline)
      end
    end
  end
  
  # Parses (p1: Type, p2: Type)
  def parse_parameters? : Array(Ast::Parameter)?
    if peek(:skip_newline).is_a?(Token::LParen)
      parse_parameters([] of Ast::Parameter)
    else
      nil
    end
  end

  def parse_parameter : Ast::Parameter
    loc = peek.location
    convention = read_convention?
    name = consume_identifier("parameter name")
    # optional colon and/or newline
    consume?(Token::Colon, :skip_newline) || consume?(Token::Newline)  
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
    @log.debug_descend(peek.location, "Parsing type arguments...") do
      type_args = [] of Ast::Type
      until eof?
        type_args << parse_type_expression
        consume?(Token::Comma) || break
      end
      consume!(Token::RBracket, :skip_newline)
      type_args
    end
  end

  # Parses `: BLOCK_OF_STATEMENTS`
  def parse_colon_and_block(block_base_indent : Int32) : Ast::Block?
    loc = peek.location
    @log.debug(loc, "Parsing colon and block (base indent: #{block_base_indent})...")
    @log.debug_descend

    consume(Token::Colon) || return nil # Expect ':'

    # Statements are parsed relative to the block_base_indent.
    # A statement belongs to the block if it's indented further than block_base_indent.
    statements_ast = parse_statements(block_base_indent)

    
    Ast::Block.new(loc: loc, statements: statements_ast) # loc could be colon's loc or start of block
  end
  
  # Parses a sequence of statements until indent decreases or block ends
  def parse_statements(current_block_indent : Int32, stop_at : StopAt = StopAt::Normal) : Array(Ast::Node)
    # TODO: This is a critical part that will involve the ExpressionParser.
    # For now, placeholder.
    @log.debug(peek.location, "parse_statements (indent: #{current_block_indent}, stop: #{stop_at}) - NOT FULLY IMPLEMENTED")
    
    statements = [] of Ast::Node
    # Loop:
    #   Skip newlines that don't decrease indent relative to current_block_indent
    #   If indent decreases or matches current_block_indent (and not first line), end of block.
    #   Parse one expression/statement.
    #   Add to statements.
    #   Handle separators (e.g., semicolon or just newline).
    
    # Example from Pony:
    # while match peek().kind
    # | RParen | RBracket | RBrace | EOF => false
    # | Newline => if peek_indent() > block_indent then next(); true else false end
    # else true end
    # do
    #   statements.push(parse_expression(...)?)
    #   try consume(Comma)? end // Or Semicolon, or just newline
    # end
    
    # For now, consume until a token that would typically end a simple block or expression sequence.
    # This is a very rough stub.
    temp_count = 0
    while !eof? && temp_count < 5 # Limit to avoid infinite loops in stub
      temp_count += 1
      tok = peek
      case tok
      # when Token::Newline
      #   # More complex indent handling needed here
      #   next_line_indent = get_indent_of_next_significant_line(1)
      #   if next_line_indent <= current_block_indent
      #     # @log.debug(tok.location, "Newline signals end of block, next indent #{next_line_indent} <= #{current_block_indent}")
      #     # break # Dedent or same indent ends block
      #   end
      #   # next_token # Consume newline if part of block
      when Token::RBrace, Token::RParen, Token::RBracket, Token::EOF # Common block enders
        break
      else
        # In a real scenario, here we'd call `parse_expression`
        # @log.debug(tok.location, "Stub parse_statements consuming: #{tok}")
        # statements << Ast::Error.new(loc: tok.location, message: "Expression parsing not implemented, saw: #{tok}")
        # next_token
        break # Exit after one "dummy" processing for now
      end
    end
    
    if statements.empty? && !eof? # If nothing was parsed, create a placeholder if needed
        # statements << Ast::Error.new(loc: peek.location, message: "Empty or unimplemented block")
    end
    statements
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

  # --- Placeholder parse methods for declarations ---
  # These will be implemented next.
  def parse_struct_declaration(decl_loc : Location) : Ast::Struct?
    @log.debug(decl_loc, "Parsing struct declaration...")
    # Pony: parse_struct() -> parse_type_header(), parse_fields(), then methods
    report_error(decl_loc, "parse_struct_declaration not yet implemented")
    nil # Placeholder
  end

  def parse_trait_declaration(decl_loc : Location) : Ast::Trait?
    @log.debug(decl_loc, "Parsing trait declaration...")
    # Pony: parse_trait() -> parse_type_header(), then methods
    report_error(decl_loc, "parse_trait_declaration not yet implemented")
    nil # Placeholder
  end

  def parse_enum_declaration(decl_loc : Location) : Ast::Enum?
    @log.debug(decl_loc, "Parsing enum declaration...")
    # Pony: parse_enum() -> parse_type_header(), parse_variants(), then methods
    report_error(decl_loc, "parse_enum_declaration not yet implemented")
    nil # Placeholder
  end

  def parse_import_declaration(decl_loc : Location) : Ast::Node? # TODO: Define Ast::Import
    @log.debug(decl_loc, "Parsing import declaration...")
    # Pony: parse_import(loc)
    report_error(decl_loc, "parse_import_declaration not yet implemented")
    nil # Placeholder
  end

  def parse_extend_declaration(decl_loc : Location) : Ast::Extend?
    @log.debug(decl_loc, "Parsing extend declaration...")
    # Pony: parse_extend(loc.col)
    report_error(decl_loc, "parse_extend_declaration not yet implemented")
    nil # Placeholder
  end

  # TODO: Implement parse_signature, parse_parameters, parse_type_expression,
  # parse_colon_and_block, parse_type_parameters, parse_constraints, etc.

  # TODO: Implement ExpressionParser class (for `parse_expression`)
  # TODO: Implement UcsParser class (for `parse_if_expression`)

end
