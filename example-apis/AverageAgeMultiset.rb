class AverageAgeMultiset < UnorderedMultiset
  @fields = [:age, :height]

  def insertPerson(age, height)
    insert({age: age, height: height})
  end

  def getAverageAge
    reduce(0, &:+) / size
  end
end
