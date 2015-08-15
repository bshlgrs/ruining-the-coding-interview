public class MaxStream  {
    class Person {
        int id;
        int priority;
        String nationality;
    }

    MagicMultiset<Person> people = new MagicMultiset<Person>();

    int insertPerson(Person person) {
        people.insert(person);
    }

    Item getRichest(String nationality) {
        return people.filter(x -> x.nationality == nationality).limitBy(x -> - x.priority, 10);
    }
}
