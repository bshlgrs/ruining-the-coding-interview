package data_structure_handlers

import java_parser.JavaParserWrapper
import java_transpiler.AstBuilder

import ast_renderers.RubyOutputter

object TestLoadingJavaDataStructure {
  def main(args: Array[String]) {
    val cu = JavaParserWrapper.parseJava(
      """
        |import big_o.*;
        |import java_transpiler.queries.Query;
        |
        |import java.util.function.BiFunction;
        |
        |// question: how should I handle constants which are macros vs fields which should actually be stored?
        |public class SimpleCAIOperationMemoizer {
        |    BigO timeForInsert = Constant.time();
        |    BigO timeForRemove = Constant.time();
        |
        |    int storedValue;
        |    int startValue;
        |    BiFunction<Integer, Integer, Integer> combiner;
        |
        |    // so much todo
        |    public SimpleCAIOperationMemoizer(BiFunction<Integer, Integer, Integer> combiner, int startValue) {
        |        this.combiner = combiner;
        |        this.startValue = startValue;
        |    }
        |
        |    void afterInitialize() {
        |        this.storedValue = startValue;
        |    }
        |
        |    void beforeInsert(int item) {
        |        this.storedValue = this.combiner.apply(this.storedValue, item);
        |    }
        |
        |    void afterInsert() {}
        |
        |    static BigO timeForQuery(Query query) {
        |      if (query.reduction() == null) {
        |          return Logarithmic.time();
        |      } else {
        |          return null;
        |      }
        |    }
        |
        |    int query() {
        |        return this.storedValue;
        |    }
        |}
      """.stripMargin)

    val structure = AstBuilder.build(cu).head
    println(structure)

    val thing = GenericDataStructureForMultiset.build(structure)

    val chimera = ChimeraMultisetClass(List(thing, thing)).toJavaClass

    println(RubyOutputter.outputClass(chimera))
  }
}
