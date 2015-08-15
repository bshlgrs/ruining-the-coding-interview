public class Example  {
    class Person {
        String gender;
        int age;
        int income;
    }

    MagicMultiset<Person> people = new MagicMultiset<Person>();

    int getAverageIncomeByAgeAndGender(int age, String gender) {
        int totalIncome = people.filter(x -> x.age == age).filter(x -> x.gender == gender)
                .sum(x -> x.income);
        int numberOfPeople = people.filter(x -> x.age == age).filter(x -> x.gender == gender).sum(x -> 1);

        return totalIncome / numberOfPeople;
    }

    int insertPerson(int age, int income) {
        people.insert(age, income);
    }

    void removePerson(Person person) {
        people.remove(person);
    }
}
