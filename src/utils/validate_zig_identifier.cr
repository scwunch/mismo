ZIG_KEYWORDS = Set{
  "align", "allowzero", "and", "anyframe", "anytype", "asm", "async", "await",
  "break", "bool", "catch", "comptime", "const", "continue", "defer", "else", "enum",
  "errdefer", "error", "export", "extern", "false", "fn", "for", "if",
  "inline", "isize", "noalias", "noinline", "nosuspend", "or", "orelse", "packed",
  "pub", "resume", "return", "struct", "suspend", "switch", "test",
  "threadlocal", "true", "try", "type", "union", "unreachable", "usingnamespace", "usize",
  "var", "void", "volatile", "while"
}

ZIG_IDENTIFIER_REGEX = /^[a-zA-Z_][a-zA-Z0-9_]*$/
ZIG_NUMTYPE_REGEX = /^(i|f|u)[1-9][0-9]?[0-9]?$/

def valid_zig_identifier?(str : String)
  !ZIG_KEYWORDS.includes?(str) && ZIG_IDENTIFIER_REGEX.match(str) && !ZIG_NUMTYPE_REGEX.match(str)
end

# class String
#   def to_zig_ident
#     if valid_zig_identifier?(self)
#         self
#     else
#         "@\"#{self}\""
#     end
#   end
#   def to_type_name
#     if valid_zig_identifier?(self)
#       self + "_t"
#     else
#       "@\"#{self}\"_t"
#     end
#   end
#   def to_function_name(overload_index : Int)
#     if valid_zig_identifier?(self)
#       self + "_" + overload_index.to_s
#     else
#       "@\"#{self}\"_#{overload_index}"
#     end
#   end
# end