enum Mode
  Move
  Mut
  Let
  Box
  Ref

  def self.from_string?(str : String)
    case str
    when "move" then Move
    when "mut" then Mut
    when "let" then Let
    when "box" then Box
    when "ref" then Ref
    # when "copy" then Copy
    else nil
    end
  end

  def self.from_string!(str)
    self.from_string? || raise "Invalid mode: #{str}"
  end
end
  
alias Convention = Mode | Nil

# Bindings
# Var
# Mut
# Let
# Box
# Ref