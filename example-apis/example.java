public class Example  {
    class Person {
        String gender;
        int age;
        int income;
    }

    MagicMultiset<Person> stuff = new MagicMultiset<Person>();

    int getAverageIncomeByAgeAndGender(int age, String gender) {
        int totalIncome = stuff.filter(x -> x.age == age).filter(x -> x.gender == gender)
                .sum(x -> x.income);
        int numberOfPeople = stuff.filter(x -> x.age == age).filter(x -> x.gender == gender).sum(x -> 1);

        return totalIncome / numberOfPeople;
    }

    int insertPerson(int age, int income) {
        stuff.insert(age, income);
    }

    void removePerson(Person person) {
        stuff.remove(person);
    }
}
