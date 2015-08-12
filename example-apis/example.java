public class Example  {
    class Person {
        public Person(int age, int income) {
            this.age = age;
            this.income = income;
        }

        int age;
        int income;
    }

    MagicMultiset<Person> stuff;

    int getAverageIncomeOfOverFifties() {
        int totalIncome = stuff.filter(x -> x.age > 50)
                    .sum(x -> x.income);
        int numberOfPeople = stuff.filter(x -> x.age > 50).sum(x -> 1); 

        return totalIncome / numberOfPeople;
    }
    
    int insertPerson(int age, int income) {
        stuff.insert(age, income);
    }

    void removePerson(Person person) {
        stuff.remove(person);
    }
}