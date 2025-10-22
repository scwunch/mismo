require "../interpreter/interpreter"
require "../type_checker/types"

def loc
  Location.zero
end

BUILTINS = [
  FunctionDef.new(
    "print", 
    Slice[TypeParameter.new(loc, "T")], 
    [Parameter.new(loc, Mode::Let, "value", Type.var(0))],
    Type.nil,
    ->(interpreter : Interpreter) {
      p interpreter.frame.variables["value"].data
      Val.nil
    }
  ),

  FunctionDef.new(
    "print",
    [Parameter.new(loc, Mode::Let, "value", Type.string)],
    Type.nil,
    ->(interpreter : Interpreter) {
      p interpreter.frame.variables["value"].data
      Val.nil
    }
  ),
  
  # Type.int
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
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
  FunctionDef.new(
    "float",
    [Parameter.new(loc, Mode::Let, "i", Type.int)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["i"].data.as(Int).to_f64)
    }
  ),
  FunctionDef.new(
    "float",
    [Parameter.new(loc, Mode::Let, "n", Type.nat)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["n"].data.as(UInt64).to_f64)
    }
  ),
  FunctionDef.new(
    "round",
    [Parameter.new(loc, Mode::Let, "x", Type.float)],
    Type.int,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Float).round.to_i)
    }
  ),
  FunctionDef.new(
    "abs",
    [Parameter.new(loc, Mode::Let, "x", Type.float)],
    Type.float,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Float).abs)
    }
  ),
  FunctionDef.new(
    "abs",
    [Parameter.new(loc, Mode::Let, "x", Type.int)],
    Type.nat,
    ->(interpreter : Interpreter) {
      Val.new(interpreter.frame.variables["x"].data.as(Int).abs.to_u64)
    }
  ),
]

def FunctionDef.new(
  name : String, 
  type_params : Slice(TypeParameter), 
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionDef.new(
    Location.zero, 
    name,
    type_params, 
    parameters, 
    return_type, 
    [Hir.native(Location.zero, thunk, return_type)]
  )
end

def FunctionDef.new(
  name : String,  
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionDef.new(
    Location.zero, 
    name,
    Slice(TypeParameter).empty, 
    parameters, 
    return_type, 
    [Hir.native(Location.zero, thunk, return_type)]
  )
end

def FunctionDef.new(
  loc : Location,
  name : String, 
  type_params : Slice(TypeParameter), 
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionDef.new(
    loc, 
    name,
    type_params, 
    parameters, 
    return_type, 
    [Hir.native(loc, thunk, return_type)]
  )
end

def FunctionDef.new(
  loc : Location,
  name : String,  
  parameters : Array(Parameter),
  return_type : Type,
  thunk : Proc(Interpreter, Val))
  FunctionDef.new(
    loc, 
    name,
    Slice(TypeParameter).empty, 
    parameters, 
    return_type, 
    [Hir.native(loc, thunk, return_type)]
  )
end