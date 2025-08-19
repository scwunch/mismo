enum Operator
  # unary mode operators
  Let
  Mut
  Ref
  Move
  Copy
  # math
  Neg
  Add
  Sub
  Mul
  Div
  Mod
  Exp
  # logical
  Not
  And
  Or
  # comparison
  Is
  IsNot
  In
  NotIn
  Eq
  Neq
  Lt
  Le
  Gt
  Ge
  # special
  Comma
  Dot
  RArrow
  # assignment
  Assign
  AddAssign
  SubAssign
  MulAssign
  DivAssign
  ModAssign
  AndAssign
  OrAssign
  ExpAssign

  def self.parse?(str : String)
    case str
    when "+" then Add
    when "-" then Sub
    when "*" then Mul
    when "/" then Div
    when "%" then Mod
    when "^", "**" then Exp
    when "not", "~" then Not
    when "&", "and" then And
    when "|", "or" then Or
    when "is" then Is
    when "is not" then IsNot
    when "in" then In
    when "not in" then NotIn
    when "==" then Eq
    when "!=" then Neq
    when "<" then Lt
    when "<=" then Le
    when ">" then Gt
    when ">=" then Ge
    when "->" then RArrow
    when "=", ":=" then Assign
    when "+=" then AddAssign
    when "-=" then SubAssign
    when "*=" then MulAssign
    when "/=" then DivAssign
    when "%=" then ModAssign
    when "&&=" then AndAssign
    when "||=" then OrAssign
    when "**=", "^=" then ExpAssign
    else
      nil
    end
  end

  def to_s
    case self
    in Let then "let"
    in Mut then "mut"
    in Ref then "ref"
    in Move then "move"
    in Copy then "copy"
    in Neg then "-"
    in Add then "+"
    in Sub then "-"
    in Mul then "*"
    in Div then "/"
    in Mod then "%"
    in Exp then "^"
    in Not then "not"
    in And then "and"
    in Or then "or"
    in Is then "is"
    in IsNot then "is not"
    in In then "in"
    in NotIn then "not in"
    in Eq then "=="
    in Neq then "!="
    in Lt then "<"
    in Le then "<="
    in Gt then ">"
    in Ge then ">="
    in Comma then ","
    in Dot then "."
    in RArrow then "->"
    in Assign then "="
    in AddAssign then "+="
    in SubAssign then "-="
    in MulAssign then "*="
    in DivAssign then "/="
    in ModAssign then "%="
    in AndAssign then "&&="
    in OrAssign then "||="
    in ExpAssign then "**="
    end
  end
end


enum Operator
  def compares?
    {Operator::Eq, Operator::Neq, Operator::Lt, Operator::Le, Operator::Gt, Operator::Ge}.includes?(self)
  end

  def ucs_branch_op?  
    self.in?({
      Operator::Eq, Operator::Neq, Operator::Lt, Operator::Le, Operator::Gt, Operator::Ge,
      Operator::Is, Operator::IsNot, Operator::And
    })
  end

  def precedence_class : PrecedenceClass
    case self
    in Or then PrecedenceClass::Or
    in And then PrecedenceClass::And
    in In, Is, IsNot, NotIn then PrecedenceClass::Comparison
    in Gt, Lt, Ge, Le, Eq, Neq then PrecedenceClass::Comparison
    in Add, Sub then PrecedenceClass::Add
    in Mul, Div, Mod then PrecedenceClass::Mul
    in Exp then PrecedenceClass::Pow
    in Not, Neg then PrecedenceClass::Unary
    in Comma then PrecedenceClass::Comma
    in Dot then PrecedenceClass::Call
    in Assign then PrecedenceClass::Assign
    in AddAssign, SubAssign, MulAssign, DivAssign, ModAssign, AndAssign, OrAssign, ExpAssign then PrecedenceClass::Assign
    in Let, Mut, Ref, Move, Copy then PrecedenceClass::Unary
    in RArrow then raise "RArrow has no precedence class"
    end
  end

  

  def relative_precedence(other : Operator) : Bind
    case {precedence_class, other.precedence_class}
    in {Prec::Call, _} then Bind::Tighter              # Prec::Call is highest, and left-associative
    in {_, Prec::Call} then Bind::Looser

    in {Prec::Assign, Prec::Assign} then Bind::Looser  # right-associative
    in {Prec::Comma, Prec::Assign} then Bind::Tighter  # comma is the only operator that works in the LHS of an assignment
    in {_, Prec::Assign} then Bind::None               # everything else require
    in {Prec::Assign, _} then Bind::Looser             # and tighter on the right
    in {Prec::Comma, _} then Bind::Looser              # and comma binds looser than everything else
    in {_, Prec::Comma} then Bind::Tighter
    
    in {Prec::Or, Prec::Or} then Bind::Tighter
    in {Prec::Or, Prec::Comparison} then Bind::Tighter
    in {Prec::Or, Prec::Unary} then Bind::Looser
    in {Prec::Or, Prec::And} then Bind::None
    in {Prec::Or, Prec::Add} then Bind::None
    in {Prec::Or, Prec::Mul} then Bind::None
    in {Prec::Or, Prec::Pow} then Bind::None
    in {Prec::And, Prec::And} then Bind::Tighter
    in {Prec::And, Prec::Comparison} then Bind::Looser
    in {Prec::And, Prec::Unary} then Bind::Looser
    in {Prec::And, Prec::Or} then Bind::None
    in {Prec::And, Prec::Add} then Bind::None
    in {Prec::And, Prec::Mul} then Bind::None
    in {Prec::And, Prec::Pow} then Bind::None
    in {Prec::Comparison, Prec::Or} then Bind::Tighter
    in {Prec::Comparison, Prec::And} then Bind::Tighter
    in {Prec::Comparison, Prec::Comparison} then Bind::None
    in {Prec::Comparison, Prec::Add} then Bind::Looser
    in {Prec::Comparison, Prec::Mul} then Bind::Looser
    in {Prec::Comparison, Prec::Pow} then Bind::Looser
    in {Prec::Comparison, Prec::Unary} then Bind::Looser
    in {Prec::Add, Prec::Comparison} then Bind::Tighter
    in {Prec::Add, Prec::Add} then Bind::Tighter
    in {Prec::Add, Prec::Mul} then Bind::Looser
    in {Prec::Add, Prec::Pow} then Bind::Looser
    in {Prec::Add, Prec::Unary} then Bind::Looser
    in {Prec::Add, Prec::Or} then Bind::None
    in {Prec::Add, Prec::And} then Bind::None
    in {Prec::Mul, Prec::Comparison} then Bind::Tighter
    in {Prec::Mul, Prec::Add} then Bind::Tighter
    in {Prec::Mul, Prec::Mul} then Bind::Tighter
    in {Prec::Mul, Prec::Pow} then Bind::Looser
    in {Prec::Mul, Prec::Unary} then Bind::Looser
    in {Prec::Mul, Prec::Or} then Bind::None
    in {Prec::Mul, Prec::And} then Bind::None
    in {Prec::Pow, Prec::Comparison} then Bind::Tighter
    in {Prec::Pow, Prec::Add} then Bind::Tighter
    in {Prec::Pow, Prec::Mul} then Bind::Tighter
    in {Prec::Pow, Prec::Pow} then Bind::None
    in {Prec::Pow, Prec::Unary} then Bind::None
    in {Prec::Pow, Prec::Or} then Bind::None
    in {Prec::Pow, Prec::And} then Bind::None
    in {Prec::Unary, Prec::Pow} then Bind::None
    in {Prec::Unary, Prec::Unary} then Bind::None
    in {Prec::Unary, _} then Bind::Tighter
    end
  end

  macro assign
    PrecedenceClass::Assign
  end

end

enum Bind
  None
  Tighter
  Looser
end

alias Prec = PrecedenceClass

enum PrecedenceClass
  Assign
  Comma
  Or
  And
  Comparison
  Add
  Mul
  Pow
  Unary
  Call
end

