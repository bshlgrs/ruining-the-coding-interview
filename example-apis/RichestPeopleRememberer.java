public class RichestPeopleRememberer  {
    class Person {
        int id;
        int income;
        String nationality;
    }

    MagicMultiset<Person> people = new MagicMultiset<Person>();

    int insertPerson(Person person) {
        people.insert(person);
    }

    Item getRichest(String nationality) {
        return people.filter(x -> x.nationality == nationality).limitBy(x -> - x.income, 10);
    }
}
