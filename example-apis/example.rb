class Example
  class Person
    def initialize(age, income)
      @age = age

      @income = income
    end
  end

  def initialize
    @stuff = MagicMultiset.new
  end

  def getAverageIncomeOfOverFifties
    totalIncome = @foo
    numberOfPeople = @bar

    (totalIncome * (numberOfPeople**-1))
  end

  def insertPerson(age, income)
    insertInto_stuff(age, income)
  end

  def insertInto_stuff(age, income)
    @foo = (@foo + item.income) if item.age > 50
    @bar = (@bar + 1) if item.age > 50

    @stuff.insert(age, income)
  end
end
