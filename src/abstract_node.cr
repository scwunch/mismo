
abstract struct IrNode
  abstract def location : Location
  abstract def to_s(io : IO)

  def inspect(io : IO)
    io << {{@type.name.stringify}} << '(' << location
    {% for ivar, i in @type.instance_vars %}
        # io << {{ivar.name.stringify}} << "="
        {% if ivar.name.id != "location" %}
          # io << "{{ivar.name.id}}=" << {{ivar.name.id}}.inspect
          io << {{ivar.name.id}}.inspect
        {% end %}
        {% if i < @type.instance_vars.size - 1 %}
          io << ", "
        {% end %}
      {% end %}
      io << ')'
    end

    # def ==(other : Node)
    #   return false unless other.is_a?(self.class)
    #   self .eq other.as(self.class)
    # end

    def ==(other)
      if other.is_a?(self)
        {% for ivar in @type.instance_vars %}
          {% if ivar.name.id != "location" %}
            return false unless @{{ivar.id}} == other.@{{ivar.id}}
          {% end %}
        {% end %}
        true
      else
        false
      end
    end

    def eq(other : self)
      {% for ivar in @type.instance_vars %}
        {% if ivar.name.id != "location" %}
          # return false unless other.responds_to?(:{{ivar.name.id}})
          return false unless {{ivar.name.id}} == other.{{ivar.name.id}}
        {% end %}
      {% end %}
      true
    end
  end