require "./spec_helper"
# require "../src/tokens"

describe Token do

  it "works" do
    token = Token.int({1, 1}, 1)
    token.should eq(Token::Int.new(Location.new(1, 1), 1))
    token.location.should eq(Location.new(1, 1))
    token.to_s.should eq("Token::Int(1)")
    
  end

  describe Location do
    it "works" do
      loc = Location.new(1, 1)
      loc.should eq(Location.new(1, 1))
      loc.should_not eq(Location.new(1, 2))
      loc.to_s.should eq("1:1")
      (loc < Location.new(1, 2)).should be_true
      (loc < Location.new(2, 1)).should be_true
      (loc < Location.new(1, 1)).should be_false
      (loc < Location.new(0, 1)).should be_false
      (loc < Location.new(0, 0)).should be_false
    end
  end

  describe KeyWord do
    it "works" do
      token = Token.keyword({1, 1}, KeyWord::If)
      token.should eq(Token::KeyWord.new(Location.new(1,1), KeyWord::If))
      token.location.should eq(Location.new(1, 1))
      token.to_s.should eq("Token::KeyWord(if)")
      token.data.to_s.should eq("if")
    end
  end
end

# p! Token.lbrace({1, 1}).is_a?(Token::GroupOpen)
# p! Token.type({1, 1}, "T").is_a?(Token::GroupOpen)
# p! Token.type({1, 1}, "T").is_a?(Token::LParen | Token::LBracket | Token::LBrace)
# p! Token.type({1, 1}, "T").is_a?(Token::Type | Token::LBracket | Token::LBrace)
# puts "----------------------"
# p! Token.lbrace({1, 1}).class <= Token::GroupOpen
# p! Token.type({1, 1}, "T").class <= Token::GroupOpen
# p! Token.type({1, 1}, "T").class <= (Token::LParen | Token::LBracket | Token::LBrace)
# p! Token.type({1, 1}, "T").class <= (Token::Type | Token::LBracket | Token::LBrace)
# puts "----------------------"
# p! Token.lbrace({1, 1}).class > Token::GroupOpen
# p! Token.type({1, 1}, "T").class > Token::GroupOpen
# p! Token.type({1, 1}, "T").class > (Token::LParen | Token::LBracket | Token::LBrace)
# p! Token.type({1, 1}, "T").class > (Token::Type | Token::LBracket | Token::LBrace)
# puts "----------------------"
# p! Token.lbrace({1, 1}).class >= Token::GroupOpen
# p! Token.type({1, 1}, "T").class >= Token::GroupOpen
# p! Token.type({1, 1}, "T").class >= (Token::LParen | Token::LBracket | Token::LBrace)
# p! Token.type({1, 1}, "T").class >= (Token::Type | Token::LBracket | Token::LBrace)