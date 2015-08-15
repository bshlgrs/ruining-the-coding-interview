public class PriorityQueue  {
    class Item {
        int id;
        int priority;
    }

    MagicMultiset<Item> queue = new MagicMultiset<Item>();

    int insertItem(Item item) {
        queue.insert(item);
    }

    Item getCheapest() {
        return queue.limitBy(x -> x.priority, 1).head;
    }

    void popCheapest() {
        Item cheapest = getCheapest();
        queue.remove(cheapest);
        return cheapest;
    }
}
