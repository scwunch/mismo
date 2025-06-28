enum Mode
  Move
  InOut
  Mut
  Let
  Box
  Ref

  def self.from_string?(str : String)
    case str
    when "move" then Move
    when "inout" then InOut
    when "mut" then Mut
    when "let" then Let
    when "box" then Box
    when "ref" then Ref
    # when "copy" then Copy
    else nil
    end
  end

  def to_binding : Binding
    case self
    in Move then Binding::Var
    in InOut then Binding::Var
    in Mut then Binding::Mut
    in Let then Binding::Let
    in Box then Binding::Box
    in Ref then Binding::Ref
    end
  end

  def self.from_string!(str)
    self.from_string? || raise "Invalid mode: #{str}"
  end
end
  
alias Convention = Mode | Nil

enum Binding
  Var
  Mut
  Let
  Box
  Ref
end