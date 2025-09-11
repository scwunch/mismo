require "../interpreter/interpreter"
require "../type_checker/types"

def loc
  Location.zero
end

BUILTINS = [
  FunctionBase.new(
    "print", 
    Slice[TypeParameter.new(loc, "T")], 
    [Parameter.new(loc, Mode::Let, "value", Type.var(0))],
    Type.nil,
    ->(interpreter : Interpreter) {
      p interpreter.frame.variables["value"].data
      Val.nil
    }
  ),

  FunctionBase.new(
    "print",
    [Parameter.new(loc, Mode::Let, "value", Type.string)],
    Type.nil,
    ->(interpreter : Interpreter) {
      p interpreter.frame.variables["value"].data
      Val.nil
    }
  ),
  
  # Type.int
  FunctionBase.new(
    "+",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) + 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "-",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) - 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "*",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) * 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "/",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) //
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),

  # Type.nat
  FunctionBase.new(
    "+",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) + 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "-",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) - 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "*",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) * 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "/",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) //
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),

  # Type.int, Type.nat and vice versa
  FunctionBase.new(
    "+",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) + 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "-",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) - 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "*",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) * 
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "/",
    [Parameter.new(loc, Mode::Let, "left", Type.int),
     Parameter.new(loc, Mode::Let, "right", Type.nat)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Int) //
        interpreter.frame.variables["right"].data.as(UInt64)
      )
    }
  ),
  FunctionBase.new(
    "+",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) + 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "-",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) - 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "*",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) * 
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),
  FunctionBase.new(
    "/",
    [Parameter.new(loc, Mode::Let, "left", Type.nat),
     Parameter.new(loc, Mode::Let, "right", Type.int)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(UInt64) //
        interpreter.frame.variables["right"].data.as(Int)
      )
    }
  ),

  # Type.float
  FunctionBase.new(
    "+",
    [Parameter.new(loc, Mode::Let, "left", Type.float),
     Parameter.new(loc, Mode::Let, "right", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Float) + 
        interpreter.frame.variables["right"].data.as(Float)
      )
    }
  ),
  FunctionBase.new(
    "-",
    [Parameter.new(loc, Mode::Let, "left", Type.float),
     Parameter.new(loc, Mode::Let, "right", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Float) - 
        interpreter.frame.variables["right"].data.as(Float)
      )
    }
  ),
  FunctionBase.new(
    "*",
    [Parameter.new(loc, Mode::Let, "left", Type.float),
     Parameter.new(loc, Mode::Let, "right", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Float) * 
        interpreter.frame.variables["right"].data.as(Float)
      )
    }
  ),
  FunctionBase.new(
    "/",
    [Parameter.new(loc, Mode::Let, "left", Type.float),
     Parameter.new(loc, Mode::Let, "right", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(
        interpreter.frame.variables["left"].data.as(Float) //
        interpreter.frame.variables["right"].data.as(Float)
      )
    }
  ),

  # numeric conversion functions
  FunctionBase.new(
    "float",
    [Parameter.new(loc, Mode::Let, "i", Type.int)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["i"].data.as(Int).to_f64)
    }
  ),
  FunctionBase.new(
    "float",
    [Parameter.new(loc, Mode::Let, "n", Type.nat)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["n"].data.as(UInt64).to_f64)
    }
  ),
  FunctionBase.new(
    "round",
    [Parameter.new(loc, Mode::Let, "x", Type.float)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Float).round.to_i)
    }
  ),
  FunctionBase.new(
    "abs",
    [Parameter.new(loc, Mode::Let, "x", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Float).abs)
    }
  ),
  FunctionBase.new(
    "abs",
    [Parameter.new(loc, Mode::Let, "x", Type.int)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Int).abs.to_u64)
    }
  ),
]

def FunctionBase.new(
  name : String, 
  type_params : Slice(TypeParameter), 
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionBase.new(
    Location.zero, 
    name,
    type_params, 
    parameters, 
    return_type, 
    [Hir.native(Location.zero, thunk, return_type)]
  )
end

def FunctionBase.new(
  name : String,  
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionBase.new(
    Location.zero, 
    name,
    Slice(TypeParameter).empty, 
    parameters, 
    return_type, 
    [Hir.native(Location.zero, thunk, return_type)]
  )
end

def FunctionBase.new(
  loc : Location,
  name : String, 
  type_params : Slice(TypeParameter), 
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionBase.new(
    loc, 
    name,
    type_params, 
    parameters, 
    return_type, 
    [Hir.native(loc, thunk, return_type)]
  )
end

def FunctionBase.new(
  loc : Location,
  name : String,  
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionBase.new(
    loc, 
    name,
    Slice(TypeParameter).empty, 
    parameters, 
    return_type, 
    [Hir.native(loc, thunk, return_type)]
  )
end