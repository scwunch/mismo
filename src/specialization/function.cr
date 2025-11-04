# monomorphized (non-generic) function
class Function
  property function_def : FunctionDef
  property type_args : Slice(Type)
  # property location : Location
  # property name : String
  property index : Int32
  # property parameters : ::Array(Parameter) = [] of Parameter
  # property return_mode : Mode = Mode::Move
  # property return_type : Type = Type.nil
  property body : Hir

  # def initialize(@location : Location, @name : String, @index : Int32, @parameters : ::Array(Parameter) = [] of Parameter, @return_mode : Mode = Mode::Move, @return_type : Type = Type.nil, @body : Hir = Hir::Block.empty)
  # end
  def initialize(@function_def : FunctionDef, @type_args : Slice(Type), @index : Int32, @body : Hir = Hir::Block.empty)
  end

  def self.blank
    new(FunctionDef.new(Location.zero, "<none>"), Slice(Type).empty, -1, Hir::Block.empty)
  end

  def location : Location
    function_def.location
  end
  def name : String
    function_def.name
  end
  def parameters : ::Array(Parameter)
    function_def.parameters.map { |param| param.substitute(type_args) }
  end
  def return_mode : Mode
    function_def.return_mode
  end
  def return_type : Type
    function_def.return_type.substitute(type_args)
  end


  def to_s(io : IO)
    io << "def "
    io << name
    io << "(#{parameters.join(", ")})"
    unless return_type == Type.nil
      io << " -> #{return_type}"
    end
    io << "\n"
    io << body
    # body.each do |hir|
    #   io << "  #{hir}\n"
    # end
  end
end