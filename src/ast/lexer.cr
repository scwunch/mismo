require "./tokens"
require "../utils/logger"

macro locator()
  "#{__FILE__}(#{__LINE__})"
end

class Lexer
  getter reader : Reader
  getter current_token : Token = Token::BeginFile.new(Location.zero)
  getter log : Logger

  def initialize(@reader : Reader, @log : Logger)
    skip_whitespace_and_comments
  end
  def initialize(text : String, file_path : String, starting_line, log_level : Logger::Level = Logger::Level::Warning)
    @reader = Reader.new(text, file_path, starting_line)
    @log = Logger.new(log_level)
    skip_whitespace_and_comments
  end

  
  class Reader
    getter source : Source
    # getter text : String  
    getter input : IO  #::Memory | IO::Buffered
    # getter idx : UInt32 = 0_u32
    property current_line : String
    getter line : UInt32
    getter col : UInt32 = 1_u32 # Represents column *before* processing char at @idx, 1-based for new lines

    def initialize(@input : IO, file_name : String?=nil, starting_line = 1)
      # source_lines = @text.split("\n")
      if file_name
        file_name = file_name.sub("/home/ryan/programming-projects/crystal/mismo/", "")
        @source = Source.empty_with_path(file_name)
      else
        @source = Source.empty
      end
      # source_lines.insert(0, file_name)
      # @text = IO::Memory.new(text)
      @line = starting_line.to_u32
      @current_line = input.gets(chomp: false) || ""
      @source << @current_line
    end
    def initialize(text : String, file_name : String?=nil, starting_line = 1)
      initialize(IO::Memory.new(text), file_name, starting_line)
    end

    def peek : Char
      current_line[col - 1]? || '\0'
        # if c = input.read_char
        #   input.pos -= c.bytesize
        #   c
        # else
        #   '\0'
        # end
      # @text.peek[0]? || '\0'
      # if @idx < @text.size
      #   @text[@idx]
      # else
      #   '\0'
      # end
    end

    # this function is only used once by the Lexer
    def peek2 : Char
      current_line[col]? || '\0'
      # pos = input.pos
      # input.pos += 1
      # if c = input.read_char
      #   input.pos = pos
      #   c
      # else
      #   input.pos = pos
      #   '\0'
      # end
      # if @idx + 1 < @text.size
      #   @text[@idx + 1]
      # else
      #   '\0'
      # end
    end

    def next : Char
      # if c = input.read_char
      #   if c == '\n'
      #     append_last_line
      #     @line += 1
      #     @col = 1
      #   else
      #     @col += 1
      #   end
      #   c
      # else
      #   '\0'
      # end
      if char = current_line[col - 1]?
        if char == '\n'
          newline
        else
          @col += 1
        end
        char
      else
        # Reader reached end of input
        '\0'
      end
      # if @idx < @text.size
      #   char = @text[@idx]
      #   @idx += 1
      #   if char == '\n'
      #     @line += 1
      #     @col = 1
      #   else
      #     @col += 1
      #   end
      #   char
      # else
      #   '\0' # End of input
      # end
    end

    def newline
      @current_line = 
        if line = input.gets(chomp: false)
          @source << line
          @line += 1
          @col = 1
          line
        else
          ""
        end
    end

    def has_next? : Bool
      # @idx < @text.size
      # idx < input.bytesize
      # peek != '\0'
      col - 1 < current_line.size
    end

    def eof? : Bool
      # idx >= input.bytesize
      peek == '\0'
    end

    # Efficiently check if the substring matches
    def peek_str?(str_to_match) : Bool
      # return false if idx + str_to_match.bytesize > input.bytesize
      # @text.unsafe_byte_slice(@idx, str_to_match.bytesize) == str_to_match.unsafe_byte_slice(0)
      # input.to_slice[idx, str_to_match.bytesize]? == str_to_match.to_unsafe_byte_slice(0)
      return false if (@col - 1 + str_to_match.size) > current_line.size
      current_line.unsafe_byte_slice(@col - 1, str_to_match.bytesize) == str_to_match.unsafe_byte_slice(0)
    end

    # Get sub-string via unsafe_byte_slice
    # def peek_str!(count) : String
    #   @text.unsafe_byte_slice(@idx, count).to_s
    # end

    # Get sub-string
    def peek_str(count) : String
      if count < 0
        current_line[@col + count - 1, @col - 1]
      else
        current_line[@col - 1, count]
      end
    end

    def location
      Location.new(@source, @line, @col)
    end

    def indent
      @col - 1
    end
  end

  def push_token(tok : Token)
    @log.debug(@reader.location, "emitting token: #{tok}")
    @current_token = tok
  end

  def lex(tokens = [] of Token)
    # tokens = [@current_token]    
    while true
      tokens << (tok = self.next)
      break if tok.is_a?(Token::EOF)
    end
    tokens
  end

  def next
    @log.debug_descend(@reader.location, "next token") do
      loc = @reader.location
      @log.debug(loc, "next: '#{@reader.peek}'")
      case @reader.peek
      when '\n'
        push_newline(loc)
      when .ascii_whitespace?
        skip_horizantal_whitespace
        return self.next
      when ','
        push_token(Token.comma(loc))
        @reader.next
      when ';'
        push_token(Token.semicolon(loc))
        # skip_whitespace_and_comments
      when ':'
        if @reader.peek_str?(":=")
          push_token(Token.operator(loc, Operator::Assign))
          @reader.next
        else
          push_token(Token.colon(loc))
        end
        @reader.next
      when '{'
        push_token(Token.lbrace(loc))
        @reader.next
        # skip_whitespace_and_comments # if context == ParserContext::List
      when '['
        push_token(Token.lbracket(loc))
        @reader.next
        # skip_whitespace_and_comments  # if context == ParserContext::List
      when '('
        push_token(Token.lparen(loc))
        @reader.next
        # skip_whitespace_and_comments  # if context == ParserContext::List
      when ')'
        push_token(Token.rparen(loc)); @reader.next
      when ']'
        push_token(Token.rbracket(loc)); @reader.next
      when '}'
        push_token(Token.rbrace(loc)); @reader.next
      when '.'
        push_token(Token.dot(loc)); @reader.next
      when '"', '\'', '`'
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
      when .ascii_letter?, '_'
        push_word(loc, read_word(loc))  # push_word consumes
      when '\0'
        if @reader.has_next?
          push_token(Token.error(loc, "unexpected null character"))
          @reader.next
        else # End of file
          case @current_token
          when Token::Newline
            push_token(Token.eof(loc))
          when Token::EOF
            nil
          else
            push_token(Token.newline(loc, 0))
          end
        end
      else
        push_token(Token.error(loc, "unrecognized character: '#{@reader.next}'"))
      end
    end
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
  def push_word(loc : Location, word : String)
    @log.debug_descend(loc, "push_word: #{word}") do
      # raise "breakpoint"
      if key = KeyWord.parse?(word)
        if loc.column == 1 && key == KeyWord::Import
          skip_horizantal_whitespace
          handle_import
        else
          push_token(Token.keyword(loc, key))
        end
      elsif is_type_name?(word)
        push_token(Token.type(loc, word))
        # @log.debug(loc, "pushed type name: #{word}")
      elsif op = Operator.parse?(word)
        push_word_operator(loc, op)
      elsif word == "else"
        push_token(Token.else(loc))
      else
        push_token(Token.variable(loc, word))
        # @log.debug(loc, "pushed variable name: #{word}")
      end
    end
  end

  def push_word_operator(loc, op)
    @log.debug_descend(loc, "push word operator: #{op}") do
      case op
      when Operator::Not
        skip_horizantal_whitespace
        if @reader.peek_str?("in")
          push_token(Token.operator(loc, Operator::NotIn))
          2.times { @reader.next }
        else
          push_token(Token.not(loc))
        end
      when Operator::Is
        skip_horizantal_whitespace
        if @reader.peek_str?("not")
          push_token(Token.operator(loc, Operator::IsNot))
          3.times { @reader.next }
        else
          push_token(Token.operator(loc, op))
        end
      else
        push_token(Token.operator(loc, op))
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
    start_idx = @reader.col - 1   
    while @reader.peek.ascii_alphanumeric? || @reader.peek == '_'
      @reader.next
    end
    word_text = @reader.current_line[start_idx...(@reader.col - 1)]
    @log.debug(loc, "read_word: #{word_text}")
    word_text
  end

  def push_string(loc : Location)
    push_string(loc, @reader.next)
  end

  def push_string(loc : Location, quote : Char)
    @log.debug_descend(loc, "push string") do
      quote_width = 1  # assume first opening quote has already been consumed
      while @reader.peek == quote
        quote_width += 1
        @reader.next
      end
      str = if quote_width >= 3
        parse_multiline_string(quote, quote_width)
      elsif quote == '`'
        parse_raw_string(quote_width)
      elsif quote_width == 2
        ""
      else
        # assert quote_width == 1
        parse_string(quote)
      end
      
        # str = case @reader.peek
        # when '"', '\''
        #   quote = @reader.next
        #   quote_width = 1
        #   while @reader.peek == quote
        #     @reader.next
        #     quote_width += 1
        #   end
        #   case quote_width
        #   when 1
        #     parse_string(@reader.next)
        #   when 2
        #     ""
        #   else
        #     parse_multiline_string(quote, quote_width)
        #   end
        # when '`' 
        #   parse_raw_string
        # else
        #   push_token(Token.error(loc, "Invalid string quote: '#{@reader.next}'"))
        #   return
        # end
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
          when '\0', '\n'
            push_token(Token.error(@reader.location - 1, "Unterminated string literal"))
            break
          else
            str << char
          end
        end
      end
    end
  end

  # function is called immediately after consumption of the opening quote
  def parse_multiline_string(quote : Char, quote_width : Int)
    @log.debug_descend(@reader.location, "parse multiline string") do
      if @reader.peek != '\n'
        push_token Token.error(@reader.location, "Multiline string start token must be followed by a newline")
      else
        @reader.next
      end
      range = get_string_lines(quote.to_s * quote_width)
      indent = @reader.indent
      quote_width.times { @reader.next }
      String.build do |str|
        if @reader.source[range]?.nil?
          log.error(@reader.location, "Multiline string range of lines out of bounds: #{range}")
          log.error(@reader.location, "source lines (#{@reader.source.size}): #{@reader.source}")
          raise "BREAK"
        end
        @reader.source[range].each_with_index do |line, idx|
          str << '\n' if idx > 0
          if quote == '`'  # raw string
            str << line[indent..-2]
          else
            unescaped(line, indent, str)
          end
        end
      end
    end
  end

  def unescaped(line : String, indent, io : IO)
    char_iterator = line.each_char
    indent.times do |i|
      case char_iterator.next
      when '\n' then return
      when ' '
      else
        push_token(Token.error(@reader.location - 1, "Invalid indent on line #{i+1} of multiline string (this also needs a more descriptive message)"))
      end
    end
    char_iterator.each do |c|
      case c
      when '\\'
        io << case char = char_iterator.next
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
      else
        break if c == '\n'
        io << c
      end
    end
  end

  # this function should be called after the opening quote has already been consumed,
  # and does not consume the end quote
  def get_string_lines(quote : String) : Range
    @log.debug_descend(@reader.location, "get string lines") do
      start = @reader.source.size
      expecting_end_quote = true
      while @reader.has_next?
        c = @reader.peek
        if expecting_end_quote
          break if @reader.peek_str?(quote)
          expecting_end_quote = c.ascii_whitespace?
        elsif c == '\n'
          expecting_end_quote = true
        end
        @reader.next
      end
      stop = @reader.source.size - 1
      Range.new(start, stop)
    end
  end

  def parse_multiline_string_deprecated(quote : Char, quote_width, deprecated) : String
    @log.debug_descend(@reader.location, "parse multiline string") do
      end_quote = quote.to_s * quote_width
      expecting_end_quote = true
      String.build do |str|
        while true
          case char = @reader.next
          when quote
            break if expecting_end_quote && @reader.peek_str?(end_quote)
            puts "NOTE: found quote (#{quote}), but not end quote (#{end_quote})"
            str << quote
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
          when '\n'
            expecting_end_quote = true
            str << '\n'
          when '\0'
            push_token(Token.error(@reader.location - 1, "Unterminated string literal"))
            break
          else
            if expecting_end_quote
              expecting_end_quote = char.ascii_whitespace?
            end
            str << char
          end
        end
      end
    end
  end

  def parse_raw_string(num_backticks) : String
    @log.debug_descend(@reader.location, "parse") do
      multiline = num_backticks >= 3
      backticks = "`" * num_backticks
      expecting_end_quote = true
      String.build do |str|
        while true
          if @reader.peek_str?(backticks) 
            backticks.size.times { @reader.next }
            if expecting_end_quote
              break
            else
              str << backticks
            end
          elsif @reader.has_next?
            str << (c = @reader.next)
            if multiline
              if expecting_end_quote
                expecting_end_quote = c.ascii_whitespace?
              elsif c == '\n'
                expecting_end_quote = true
              end
            elsif c == '\n'
              push_token Token.error(@reader.location, "Unterminated string literal")
            end
          else
            push_token(Token.error(@reader.location, "Unterminated string literal"))
            break
          end
        end
      end
    end
  end
  
  @[Deprecated("Use `#parse_raw_string(num_backticks : Int)` instead")]
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
        # @out.import(import_loc, path, import_alias, import_bindings)
        @log.info(import_loc, "Lexer.handle_import => import #{path}, alias: #{import_alias}, bindings: #{import_bindings}")
      end
      skip_whitespace_and_comments
    end
  end

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

class String
  def strip_margin(margin) : String
    self.each_line.map { |line| line[margin..] }
    .join '\n'
  end
end