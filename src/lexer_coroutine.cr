require "./tokens" # Assumed to define Token, Location, Keyword, Operator, ControlWord, TokenKind, Conventions etc.
require "./logger"
require "./ast_nodes"

class Lexer
  getter reader : Reader
  @current_token : Token = Token::BeginFile.new(Location.zero)
  @next_token : Token? = nil
  getter log : Logger
  getter tokens_emitted = 0

  def initialize(@reader : Reader, @log : Logger)
    next
  end
  def initialize(text : String, @log : Logger)
    @reader = Reader.new(text)
    next
  end

  class Reader
    getter text : String
    getter idx : UInt32 = 0_u32
    getter line : UInt32 = 1_u32
    getter col : UInt32 = 1_u32 # Represents column *before* processing char at @idx, 1-based for new lines

    def initialize(@text : String)
    end

    def peek : Char
      if @idx < @text.size
        @text[@idx]
      else
        '\0'
      end
    end

    def peek2 : Char
      if @idx + 1 < @text.size
        @text[@idx + 1]
      else
        '\0'
      end
    end

    def next : Char
      if @idx < @text.size
        char = @text[@idx]
        @idx += 1
        if char == '\n'
          @line += 1
          @col = 1
        else
          @col += 1
        end
        char
      else
        '\0' # End of input
      end
    end

    def has_next? : Bool
      @idx < @text.size
    end

    def eof? : Bool
      @idx >= @text.size
    end

    def peek_str?(str_to_match) : Bool
      return false if @idx + str_to_match.bytesize > @text.bytesize
      # Efficiently check if the substring matches
      @text.unsafe_byte_slice(@idx, str_to_match.bytesize) == str_to_match.unsafe_byte_slice(0)
    end

    def peek_str!(count) : String
      @text.unsafe_byte_slice(@idx, count).to_s
    end

    def peek_str(count) : String
      if count < 0
        @text[@idx + count, @idx]
      else
        @text[@idx, count]
      end
    end

    # Note: Original iter! and iter methods from user code are removed as they are not used in the lexer logic below.
    # If they are needed elsewhere, they can be re-added.

    def location
      # Location should represent the start of the current token being processed
      # `col` is updated *after* a char is processed by `next`.
      # So, if `next` was just called, `col - 1` might be the char's col.
      # However, Pony's `reader.location()` usually means current read head.
      Location.new(@line, @col)
    end
  end

  def push_token(tok : Token)
    @log.debug(@reader.location, "emitting token: #{tok}")
    @tokens_emitted += 1
    @current_token = @next_token
    @next_token = tok
  end

  def next
    @log.debug_descend(@reader.location, "next token") do
      loc = @reader.location
      @log.debug(loc, "next: '#{@reader.peek}'")
      case @reader.peek
      when '\n'
        push_newline
      when .ascii_whitespace?
        @reader.next
      when ','
        push_token(Token.comma(loc)); @reader.next
      when ';'
        push_token(Token.semicolon(loc)); @reader.next
      when ':'
        push_token(Token.colon(loc)); @reader.next
      when '{'
        push_token(Token.lbrace(loc)); @reader.next
      when '['
        push_token(Token.lbracket(loc)); @reader.next
      when '('
        push_token(Token.lparen(loc)); @reader.next
      when ')'
        push_token(Token.rparen(loc)); @reader.next
      when ']'
        push_token(Token.rbracket(loc)); @reader.next
      when '}'
        push_token(Token.rbrace(loc)); @reader.next
      when '.'
        push_token(Token.dot(loc)); @reader.next
      when '"', '\''
        push_string(loc) # push_string consumes
      when '0'..'9'
        push_number(loc) # push_number consumes
      when '~', '!', '@', '#', '$', '%', '^', '&', '*', '+', '-', '/', '?', '<', '>', '|', '='
        # Check for comment "--"
        if @reader.peek_str?("--")
          skip_comment
          push_newline
        else
          push_operator(loc) # push_operator consumes
        end
      when .ascii_letter?
        push_word(loc)  # push_word consumes
      when '\0'
        if @reader.has_next?
          push_token(Token.error(loc, "unexpected null character"))
          @reader.next
        else # End of file
          break
        end
      else
        push_token(Token.error(loc, "unrecognized character: '#{@reader.next}'"))
      end
    end
    @log.info(@reader.location, "emitted #{@tokens_emitted} tokens")
    @tokens_emitted = 0
    @current_token
  end

  # Consumes the remainder of the current line, but does not consume the newline.
  def skip_comment
    @log.debug(@reader.location, "skipping comment...")
    until @reader.peek == '\n' || @reader.peek == '\0'
      @reader.next
    end
  end

  # Consumes all horizontal whitespace characters (spaces and tabs)
  def skip_horizantal_whitespace
    @log.debug(@reader.location, "skipping whitespace...")
    while @reader.peek == ' ' || @reader.peek == '\t'
      @reader.next
    end
  end

  # Consumes all comments and whitespace (including '\n') until the next non-whitespace character
  def skip_whitespace_and_comments
    @log.debug_descend(@reader.location, "skipping whitespace and comments...") do
      while @reader.has_next?
        if @reader.peek.ascii_whitespace?
          @reader.next
        elsif @reader.peek_str?("--")
          skip_comment
        else
          break
        end
      end
    end
  end

  # Skip over all whitespace and comments until the next significant character, 
  # and pushes a newline token
  def push_newline(loc : Location = @reader.location)
    @log.debug_descend(loc, "pushing newline") do
        skip_whitespace_and_comments
      if @reader.has_next?
        push_token(Token.newline(loc, @reader.col - 1))
      else
        push_token(Token.newline(loc, 0))
      end
    end
  end

  # handle the tokenization of a given word at the given location
  def push_word(loc : Location, word : String? = nil)
    word ||= read_word(loc)
    @log.debug_descend(loc, "handle word: #{word}") do
      if key = KeyWord.parse?(word)
        if loc.column == 1 && key == KeyWord::Import
          skip_horizantal_whitespace
          handle_import
        elsif loc.column > 1 && key.top_level_keyword?
          # certain keywords are only allowed at the top level
          push_token(Token.variable(loc, word))
        else
          push_token(Token.keyword(loc, key))
        end
      elsif is_type_name?(word)
        push_token(Token.type(loc, word))
        # @log.debug(loc, "pushed type name: #{word}")
      elsif op = Operator.parse?(word)
        push_word_operator(loc, op)
      else
        push_token(Token.variable(loc, word))
        # @log.debug(loc, "pushed variable name: #{word}")
      end
    end
  end

  def push_word_operator(loc, op)
    @log.debug_descend(loc, "push word operator: #{op}") do
      if op == Operator::Not && (prev = @current_token) && prev.data == Operator::Is
        @current_token = Token.operator(prev.location, Operator::IsNot)
        @log.debug(loc, "pushed 'is not' operator")
      elsif op == Operator::In && (prev = @current_token) && prev.data == Operator::Not
        @current_token = Token.operator(prev.location, Operator::NotIn)
        @log.debug(loc, "pushed 'not in' operator")
      else
        push_token(Token.operator(loc, op))
        # @log.debug(loc, "pushed operator: #{op}")
      end
    end
  end

  # Returns true if the word starts with an uppercase letter, or begins 
  # with any number of underscores followed by an uppercase letter.
  private def is_type_name?(word : String) : Bool
    word.each_char do |c|
      case c
      when '_' then next
      when .ascii_uppercase? then return true
      else return false
      end
    end
    false
  end

  def push_number(loc : Location)
    @log.debug_descend(loc, "push number") do
      case num = parse_number
      in Int128
        @log.debug(loc, "parse_number # => #{num}")
        push_token(Token.int(loc, num))
      in Float64
        @log.debug(loc, "parse_number # => #{num}")
        push_token(Token.float(loc, num))
      end
    end
  end
  
  def parse_number : (Int128 | Float64)
    is_float = false
    int_part = 0_i128
    float_part = 0_f64
    while true
      case @reader.peek
      when '0'..'9'
        int_part *= 10
        int_part += @reader.next.to_i
      when '.'
        if @reader.peek2.ascii_number?
          @reader.next
          is_float = true
          decimal_places = 1
          float_part = 0_f64
          while true
            case @reader.peek
            when '0'..'9'
              decimal_places *= 10
              float_part += @reader.next.to_i / decimal_places
            when '_'
              @reader.next
            else
              break
            end
          end
        end
        break
      when '_'
        @reader.next
      else
        break
      end
    end

    if is_float
      int_part + float_part
    else
      int_part
    end
  end

  def push_operator(loc : Location)
    @log.debug_descend(loc, "push operator") do
      # Try to match longest possible operator first (e.g., 3 chars, then 2, then 1)
      3.downto(1) do |i|
        op_str = @reader.peek_str(i)
        if op = Operator.parse?(op_str)
          push_token(Token.operator(loc, op))
          op_str.size.times { @reader.next }
          return
        end
      end
      unrecognized_op_char = @reader.next
      push_token(Token.error(loc, "Unrecognized operator starting with: '#{unrecognized_op_char}'"))
    end
  end

  def read_word(loc : Location = @reader.location) : String
    start_idx = @reader.idx
    while @reader.peek.ascii_alphanumeric? || @reader.peek == '_'
      @reader.next
    end
    word_text = @reader.text[start_idx...@reader.idx]
    @log.debug(loc, "read_word: #{word_text}")
    word_text
  end

  # deprecated — this logic should be moved to parser
  # def read_function_name
    # loc = @reader.location
    # start_idx = @reader.idx

    # # Log initial part for debugging
    # peek_len = Math.min(8, @reader.text.size - (@reader.idx || 0)) # Ensure @reader.idx is not nil
    # @log.debug(loc, "read_function_name starting with: '#{@reader.text[start_idx, peek_len]}'")


    # if is_operator_char?(@reader.peek)
    #   while is_operator_char?(@reader.peek)
    #     @reader.next
    #   end
    #   name_text = @reader.text[start_idx...@reader.idx]
    #   # Function names can be operators (e.g. `def +()`)
    #   push_token(Token.variable_name(loc, name_text))
    # elsif is_alphanumeric?(@reader.peek) # Standard alphanumeric name
    #   while is_alphanumeric?(@reader.peek)
    #     @reader.next
    #   end
    #   name = @reader.text[start_idx...@reader.idx]
    #   @log.debug(loc, "prev_tok_kind=#{prev_tok_kind.to_s}, name=#{name}, peek='#{@reader.peek_str(1)}'")

    #   is_prev_dot = prev_tok_kind == TokenKind::Dot # Assumes TokenKind::Dot

    #   if @reader.peek == '=' && !is_prev_dot
    #     # Special case for property setters like `def property=(value Type)`
    #     @reader.next # Consume '='
    #     full_name = @reader.text[start_idx...@reader.idx] # Name includes '='
    #     @log.debug(loc, "special function name: #{full_name}")
    #     push_token(Token.variable_name(loc, full_name))
    #   else
    #     # Check if previous token was 'def' keyword
    #     prev_tok_is_def_keyword = false
    #     if prev_t = @current_token
    #       # Assumes Token::Keyword has kw_type and Keyword::Type::Def enum
    #       if prev_t.is_a?(Token::Keyword) && prev_t.kw_type == Keyword::Type::Def
    #          prev_tok_is_def_keyword = true
    #       end
    #     end

    #     # Assumes a Conventions module/class
    #     is_convention = Conventions.parse?(name).is_a?(String) # Check if not nil

    #     if prev_tok_is_def_keyword && is_convention
    #       @log.debug(loc, "convention #{name} detected")
    #       push_token(Token.variable_name(loc, name)) # Push current name (e.g., "+=")
    #       skip_whitespace
    #       return read_function_name # Recursive call for actual name after convention
    #     elsif is_type_name?(name)
    #       push_token(Token.type_name(loc, name))
    #     else
    #       push_token(Token.variable_name(loc, name))
    #     end
    #   end
    # else
    #   @log.warn(loc, "read_function_name: expected operator or alphanumeric, found '#{@reader.peek}'. No token pushed.")
    #   # To prevent infinite loops if called when no valid char is present, push an error and advance.
    #   # However, callers (handle_dot, handle_word_top_level) usually ensure a valid start.
    #   # If this else is reached, it's likely an issue with caller logic or an unexpected state.
    #   # For safety, if we are stuck:
    #   if start_idx == @reader.idx && @reader.has_next? # No progress and not EOF
    #      push_token(Token.error(loc, "Failed to read function name, unexpected char: '#{@reader.peek}'"))
    #      @reader.next
    #   end
    # end
  # end

  def push_string(loc : Location)
    @log.debug_descend(loc, "push string") do
      str = case @reader.peek
      when '"', '\'' then parse_string(@reader.next)
      when '`' then parse_raw_string
      else
        push_token(Token.error(loc, "Invalid string quote: '#{@reader.next}'"))
        return
      end
      push_token(Token.string(loc, str))
    end
  end

  def parse_string(quote : Char) : String
    @log.debug_descend(@reader.location, "parse string") do
      String.build do |str|
        while true
          case char = @reader.next
          when quote
            break
          when '\\'
            str << case char = @reader.next
              when quote then quote
              when '\\' then '\\'
              when 'n' then '\n'
              when 't' then '\t'
              when 'r' then '\r'
              when 'f' then '\f'
              when 'b' then '\b'
              # when 'u' then 
              else
                push_token(Token.error(@reader.location - 1, "Invalid escape sequence: '\\#{char}"))
                char
              end
          when '\0'
            push_token(Token.error(@reader.location - 1, "Unterminated string literal"))
            break
          else
            str << char
          end
        end
      end
    end
  end

  def parse_raw_string : String
    @log.debug_descend(@reader.location, "parse raw string") do
      backticks = String.build do |str|
        while @reader.peek == '`'
          str << @reader.next
        end
      end
      String.build do |str|
        while true
          if @reader.peek_str?(backticks)
            backticks.size.times { @reader.next }
            break
          elsif @reader.has_next?
            str << @reader.next
          else
            push_token(Token.error(@reader.location, "Unterminated string literal"))
            break
          end
        end
      end
    end
  end

  def parse_whitespace_delimited_string : String
    @log.debug_descend(@reader.location, "parse whitespace delimited string") do
      String.build do |str|
        until @reader.peek.ascii_whitespace? || @reader.eof?
          case char = @reader.next
          when '\\'
            str << case char = @reader.next
              when ' ' then ' '
              when '\t' then '\t'
              when '\\' then '\\'
              when 'n' then '\n'
              when 't' then '\t'
              when 'r' then '\r'
              when 'f' then '\f'
              when 'b' then '\b'
              # when 'u' then 
              else
                @log.error(@reader.location - 1, "Invalid escape sequence: '\\#{char}")
                char
              end
          else
            str << char
          end
        end
      end
    end
  end

  # these three helper functions are all deprecated because the 
  # logic for reading function names has been moved to the parser
    # def is_alphanumeric?(char : Char) : Bool
      # char.alphanumeric? || char == '_'
    # end

    # def is_operator_char?(char : Char) : Bool
      # case char
      # when '~', '!', '@', '#', '$', '%', '^', '&', '*', '+', '-', '/', '?', '<', '>', '|', '='
      #   true
      # else
      #   false
      # end
    # end

    # def prev_tok_kind : Token.class
      # if tok = @current_token
      #   tok.class
      # else
      #   Token::BeginFile
      # end
    # end

  #
  # parse_import is called when the reader encounters an "import" keyword
  # at the top level.  It parses the import statement and sends it to the 
  # compiler directly without the need for a separate parser.
  def handle_import(path = "", column = 1)
    @log.debug_descend(@reader.location, "handle_import") do
      import_loc = @reader.location
      path += parse_path
      if path.ends_with?('/')  # import branches
        skip_whitespace_and_comments
        if @reader.peek == '{'
          @log.error(@reader.location, "Incomplete path; import paths cannot end with '/'.  Try '/*' or '/module_name' instead.")
          return
        end
        while @reader.col > column && @reader.has_next?
          handle_import(path, @reader.col)
        end
      else                    # import leaf
        import_alias = parse_import_alias
        import_bindings = parse_import_bindings
        @log.debug(import_loc, "Lexer.handle_import => #{import_loc} #{path}, alias: #{import_alias}, bindings: #{import_bindings}")
        @out.import(import_loc, path, import_alias, import_bindings)
      end
      skip_whitespace_and_comments
    end
  end

  # def parse_import(import_loc : Location, path : String) : Ast::Import
  #   import_alias = parse_import_alias
  #   import_bindings = parse_import_bindings
  #   Ast::Import.new(import_loc, path, import_alias, import_bindings)
  # end
    

  def parse_path : String
    @log.debug_descend(@reader.location, "parse_path") do
      delim = case @reader.peek
      when '"', '\''
        parse_string(@reader.next)
      else
        parse_whitespace_delimited_string
      end
    end
  end

  def parse_import_alias : String?
    @log.debug_descend(@reader.location, "parse_import_alias") do
      skip_horizantal_whitespace
      if @reader.peek_str?("as")
        @reader.next
        @reader.next
        skip_horizantal_whitespace
        return read_word
      end
    end
    nil
  end

  def parse_import_bindings : Array({String, String})?
    @log.debug_descend(@reader.location, "parse_import_bindings") do
      skip_horizantal_whitespace
      loc = @reader.location
      if @reader.peek == '{'
        @reader.next
        skip_whitespace_and_comments
        bindings = [] of {String, String}
        while true
          binding = read_word
          import_alias = parse_import_alias || binding
          bindings << {binding, import_alias}
          case @reader.peek
          when ','
            @reader.next
            skip_horizantal_whitespace
          when '}'
            @reader.next
            return bindings
          when '\0'
            @log.error(@reader.location, "Unterminated import bindings.")
            return bindings
          else
            @log.error(loc, "Unexpected character: #{@reader.peek}; expected 'as' or comma.")
            return bindings
          end
        end
      else
        nil
      end
    end
  end
end

