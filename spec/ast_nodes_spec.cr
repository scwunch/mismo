require "./spec_helper"
# require "../src/ast_nodes"

def loc(line, column)
  Location.new(line, column)
end
def loc 
  Location.zero
end

describe Ast::Expr do
  describe "#==" do
    it "returns true if the nodes are equal type and equal fields" do
      Ast::Nil.new(loc).should eq(Ast::Nil.new(loc))
      Ast::True.new(loc).should eq(Ast::True.new(loc))
      Ast::Int.new(loc, 123).should eq(Ast::Int.new(loc, 123))
      Ast::Float.new(loc, 123.456).should eq(Ast::Float.new(loc, 123.456))
      Ast::String.new(loc, "hello").should eq(Ast::String.new(loc, "hello"))
      Ast::Let.new(loc, "x", Ast::Int.new(loc, 123)).should eq(Ast::Let.new(loc, "x", Ast::Int.new(loc, 123)))
      Ast::Var.new(loc, "x").should eq(Ast::Var.new(loc, "x"))
      Ast::Type.new(loc, "String").should eq(Ast::Type.new(loc, "String"))
      Ast::Binop.new(Ast::Int.new(loc, 123), Operator::Add, Ast::Int.new(loc, 456)).should eq(Ast::Binop.new(Ast::Int.new(loc, 123), Operator::Add, Ast::Int.new(loc, 456)))
      Ast::NotNode.new(loc, Ast::Int.new(loc, 123)).should eq(Ast::NotNode.new(loc, Ast::Int.new(loc, 123)))
      Ast::NegNode.new(loc, Ast::Int.new(loc, 123)).should eq(Ast::NegNode.new(loc, Ast::Int.new(loc, 123)))
      # Ast::DotNode.new(Ast::Int.new(loc, 123), "x").should eq(Ast::DotNode.new(Ast::Int.new(loc, 123), "x"))
      # Ast::DotNode.new("x").should eq(Ast::DotNode.new("x"))
      Ast::Call.new(loc, "x", [Ast::Int.new(loc, 456).as(Ast::Expr)]).should eq(Ast::Call.new(loc, "x", [Ast::Int.new(loc, 456).as(Ast::Expr)]))
      # Ast::MethodCall.new(Ast::Int.new(loc, 123), "foo", [Ast::Int.new(loc, 456).as(Ast::Expr)]).should eq(Ast::MethodCall.new(Ast::Int.new(loc, 123), "foo", [Ast::Int.new(loc, 456).as(Ast::Expr)]))
      # Ast::If.new(loc, Ast::Int.new(loc, 123), Ast::Int.new(loc, 456), Ast::Int.new(loc, 789)).should eq(Ast::If.new(loc, Ast::Int.new(loc, 123), Ast::Int.new(loc, 456), Ast::Int.new(loc, 789)))
      Ast::ForLoop.new(loc, Ast::Int.new(loc, 123), Ast::Int.new(loc, 456), [Ast::Int.new(loc, 789).as(Ast::Expr)]).should eq(Ast::ForLoop.new(loc, Ast::Int.new(loc, 123), Ast::Int.new(loc, 456), [Ast::Int.new(loc, 789).as(Ast::Expr)]))
      Ast::WhileLoop.new(loc, Ast::Int.new(loc, 123), [Ast::Int.new(loc, 456).as(Ast::Expr)]).should eq(Ast::WhileLoop.new(loc, Ast::Int.new(loc, 123), [Ast::Int.new(loc, 456).as(Ast::Expr)]))
      Ast::Return.new(loc, Ast::Int.new(loc, 123)).should eq(Ast::Return.new(loc, Ast::Int.new(loc, 123)))
      Ast::Break.new(loc).should eq(Ast::Break.new(loc))
      Ast::Continue.new(loc).should eq(Ast::Continue.new(loc))
      Ast::Error.new(loc, "hello").should eq(Ast::Error.new(loc, "hello"))
      Ast::TypeParameter.new(loc, "T").should eq(Ast::TypeParameter.new(loc, "T"))
      Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "String")).should eq(Ast::Parameter.new(loc, "x", Ast::Type.new(loc, "String")))
    end
  end
end