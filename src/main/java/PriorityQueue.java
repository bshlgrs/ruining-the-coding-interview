//public class PriorityQueue  {
//    class Item {
//        int priority;
//        int id;
//    }
//
//    MagicMultiset stuff = new MagicMultiset<Item>(true, true);
//
//    int getIdOfCheapest() {
//        return stuff.orderDescendingBy(x -> x.priority).first.id;
//    }
//
//    int insertItem(int priority, int id) {
//        stuff.insert(priority, id);
//    }
//
//    int popCheapest() {
//        int cheapest = getIdOfCheapest();
//        stuff.remove(cheapest);
//        return cheapest;
//    }
//}
