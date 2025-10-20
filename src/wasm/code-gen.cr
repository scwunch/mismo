require "../type_checker/hir_nodes"
require "../ast/ast_nodes"
require "../type_checker/types"
require "../type_checker/type_checker"
require "../type_checker/type_env"




abstract struct Hir
  # def Node.emit : Wasm::Node
  #   case self
  #   when Nil then Wasm::Nop.new
  #   when Int then Wasm::Const.new(value)
  #   end
  # end

  abstract def emit : Wasm::Node

  def emit : Wasm::Node
    raise "not implemented"
  end

  def Nil.emit : Wasm::Node
    Wasm::Nop.new
  end

  def False.emit : Wasm::Node
    Wasm::Const.new(0_i32)
  end

  def True.emit : Wasm::Node
    Wasm::Const.new(1_i32)
  end

  def Int.emit : Wasm::Node
    Wasm::Const.new(value)
  end

  def String.emit : Wasm::Node
    raise "not implemented"
  end

  def Identifier.emit : Wasm::Node
    Wasm::Local.get(name)
  end

  def Array.emit : Wasm::Node
    raise "not implemented"
  end
end