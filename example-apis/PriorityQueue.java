public class PriorityQueue  {
    class Item {
        public Item(int id, int priority) {
            this.id = id;
            this.priority = priority;
        }

        int id;
        int priority;
    }

    MagicMultiset<Item> queue = new MagicMultiset<Item>();

    int insertItem(int id, int priority) {
        stuff.insert(id, priority);
    }

    Item getCheapest() {
        return queue.limitBy(x -> x.priority, 1).head;
    }

    void popCheapest() {
        Item cheapest = getCheapest();
        stuff.remove(cheapest);
        return cheapest;
    }

    // if this isn't commented out, my code doesn't know how to optimize getAverageIncomeOfOverFifties.
    // void removePerson(Person person) {
    //     stuff.remove(person);
    // }
}
