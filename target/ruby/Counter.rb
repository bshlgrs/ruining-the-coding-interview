class Counter
  def initialize(start)
    @x = start

    Counter.new(->(x) { x })
    nil
  end

  def increase(y)
    @x = Sum(Set(@x, y))
    nil
  end

  def get
    @x
  end
end
