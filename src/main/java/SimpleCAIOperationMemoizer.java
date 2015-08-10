import big_o.*;
import java_transpiler.queries.UnorderedQuery;

import java.util.function.BiFunction;

// question: how should I handle constants which are macros vs fields which should actually be stored?
public class SimpleCAIOperationMemoizer {
    BigO timeForInsert = Constant.time();
    BigO timeForRemove = Constant.time();

    int storedValue;
    int startValue;
    BiFunction<Integer, Integer, Integer> combiner;

    // so much todo
    public SimpleCAIOperationMemoizer(BiFunction<Integer, Integer, Integer> combiner, int startValue) {
        this.combiner = combiner;
        this.startValue = startValue;
    }

    void initialize() {
        this.storedValue = startValue;
    }

    void beforeInsert(int item) {
        this.storedValue = this.combiner.apply(this.storedValue, item);
    }

    void afterInsert() {}

    BigO timeForQuery(UnorderedQuery query) {
      if (query.reduction() == null) {
          return Logarithmic.time();
      } else {
          return null;
      }
    }

    int query() {
        return this.storedValue;
    }
}
