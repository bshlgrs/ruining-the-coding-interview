# Ruining the coding interview

*This code is very much a work in progress. You can get a simple demo from running it with `sbt run`: by default this optimizes the code in `example-apis/example.java`.*

*I presented on this at Scala By The Bay on August 15th, 2015. [Here's my slides](https://docs.google.com/presentation/d/1G0gkzDejLqmC8KhGMXDr3CEYym8vwHxrMShkw1ps4x0/edit?usp=sharing). I'll add a video here when the videos are put up.*

When I was interviewing for my current job, I spent a lot of time learning about different data structures and algorithms, and when you’d want to use one over another. You know, standard stuff. It takes log(n) time to insert items into a binary search tree, but then you can search for them in log(n) time. It takes constant time to append an item to an array, but looking for the item then takes linear time. So when you’re deciding whether to use an array or a binary search tree, you have to look at what operations you need to support, and how regularly you’re going to call the different operations, and then you want to choose the data structure which minimizes your cost.

Other problems kind of have this structure too. Here’s a simple dynamic programming question: Given a list of the price of a stock on different days, and the restriction that you have to buy before you sell, choose the pair of dates on which you want to buy and sell to maximize your profit.

The simple implementation is to run over all pairs of dates and choose the pair with the largest price difference. This takes quadratic time. It’s a better idea to loop over the list once, keeping track of the lowest price you’ve seen so far and the best profit opportunity you’ve seen so far. This implementation runs in linear time.

We can look at this in a similar kind of way. Why is the second way faster? You can basically look at it as a way of speeding up the query “What is the lowest price occurring in the first n places in the array?”. This query is worth speeding up, because it takes linear time and we call it a linear number of times.

So the way that you solve algorithms problems is you figure out the operations which need to be called most regularly, and you figure out what to precompute to speed them up.

(Some algorithms problems don’t fit into this structure. This structure only describes the core of the algorithm question in the case where you know how to write a trivial solution to the problem, and the goal of the interview question is to come up with a faster implementation.)

Compilers do all sorts of optimizations which humans don’t want to. For example, if you write something in C like

    int mathyThing(int k) {
      int total = 0;
      int i;

      for (i = 0; i < 10; i++) {
        total += i * k * 3
      }
    }

your compiler will notice the repeated k * 3 multiplication and move that outside the loop. The compiler *spotted a slow operation and moved it outside a critical loop*. There’s that pattern again.

So my project is to make optimizing compilers at a higher level than that: at the level of choosing data structures and algorithms, rather than at the level of choosing where to compute loop invariants. This is obviously a highly ambitious project.

For this presentation I’m just going to be talking about the part of this I’ve implemented. Programmers should be able to use high level data structures like multisets and have a compiler decide what data structures the multisets should be using under the hood. I’m trying to make that happen.

Here’s the only problem. My adventures into automatically generating data structures lead me to a wide variety of complicated data structures, which I find fascinating but I feel very little desire to actually implement. So while I’m interested in the question of what data structures you should use, I’m not actually interested in doing all the hard work which I’d need to do to actually generate usable code. In real life, you’re not allowed to just say “and now you use a heap here”, but in coding interviews you are.

So for now, I’m not trying to improve programming. I just want to ruin the coding interview.


## Data structure optimizer

Okay, so how are we going to do this?

For the moment, I'm only going to talk you through how I'm doing this for unordered data structures, because they're simpler and I've done more work implementing them.

(I’ve ended up with this project compiling Java to Ruby. You might point out that this is kind of weird for a Scala project, and you’d be right. One reason I’m currently going Java to Ruby is that Java has explicit types, which makes it easier for me to reason about, and Ruby doesn’t, which makes it easier for me to output it.)

Choosing the data structures happens over several stages, just like a normal compiler. I'll walk you through how they fit together.

But first, what exactly do I mean by a "data structure"? Here are a few examples:

* If you want to store the total income of everyone who is in your collection, you can just store a single field with the total, and you can increase this by the income of the new person whenever someone is added, and you can decrease it by the income of the new person whenever someone is removed.
* You can put all of the people in your collection in a binary search tree ordered by income. Every time someone is added to your main collection, you insert them into this BST, and every time someone is removed, you remove them.
* If you don't need to be able to remove people, you could have a min heap with the ten richest users stored in it. Whenever you try to insert a new user, you see if they're richer than the root of the min heap, and if so you insert them into the heap.

So the essence of what I mean by "data structure" has:

* some fields, where it stores a nice representation of some part of your data
* if it allows insertion, some code which needs to get executed when you insert a new item
* if it allows removal, some code which needs to get executed when you remove an item
* some code that should be called to get the answer to queries.

Each of these corresponds to a method of the `UsefulUnorderedDataStructure` class:

    abstract class UsefulUnorderedDataStructure(query: UnorderedQuery) {
      def insertionFragment: Option[List[JavaStatement]]
      def removalFragment: Option[List[JavaStatement]]
      def fields: List[JavaFieldDeclaration]
      def queryCode: JavaExpressionOrQuery
    }


Okay, so here’s an example of a simple Java class. It stores a list of people. You can add people to it, and whenever you want you can ask it to tell you the average income of people who are older than fifty:

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

        int averageIncomeOfOverFifties() {
            int totalIncome = stuff.filter(x -> x.age > 50)
                        .sum(x -> x.income);
            int numberOfPeople = stuff.filter(x -> x.age > 50).sum(x -> 1); 

            return totalIncome / numberOfPeople;
        }

        int insertItem(int age, int income) {
            stuff.insert(age, income);
        }
    }

(This example totally works, by the way.)

If we naively translate this to Ruby, by using the unoptimizing Java to Ruby transpiler in this code, we end up with the following Ruby:

    class Example
      class Person
        def initialize(age, income)
          @age = age

          @income = income
        end
      end

      def initialize
        @stuff = MagicMultiset.new
      end

      def getAverageIncomeOfOverFifties
        totalIncome = stuff.select(->(x) { x.age > 50 }).map(->(x) { x.income }).inject(0, ->(x, y) { (x + y) })
        numberOfPeople = stuff.select(->(x) { x.age > 50 }).map(->(_x) { 1 }).inject(0, ->(x, y) { (x + y) })

        (totalIncome * (numberOfPeople**-1))
      end

      def insertPerson(age, income)
        insertInto_stuff(age, income)
      end

      def insertInto_stuff(age, income)
        @stuff.insert(age, income)
      end
    end

With that implementation, `getAverageIncomeOfOverFifties` takes linear time. We can do better.

Here's the main method of my project, in `Optimizer.scala`:

    def main(args: Array[String]) {
      val javaSource = Source.fromFile(args.headOption.getOrElse("example-apis/example.java")).getLines().mkString("\n")

      val javaClass = JavaParserWrapper.parseJavaClassToAst(javaSource)

      val optimizedClass = optimize(javaClass)

      val rubyCode = RubyOutputter.outputClass(optimizedClass)

      println(rubyCode)
    }

So we get a file, parse it to Java, optimize it, then output the class to Ruby. This pipeline is attractive to me because it suggests that it should be easy to output to other languages than Ruby.

Nothing that exciting happens in the `JavaParserWrapper.parseJavaClassToAst` method, except me tearing my hair out over all the `null`s in the Java library I'm interfacing with. And the `RubyOutputter.outputClass` method is marginally interesting, but not the focus of this project. So let's look at how the `Optimizer.optimize` method works:


    def optimize(jc: JavaClass): JavaClass = {
      val querified = jc.querify()

      val auxiliaryDataStructures = querified.queries().map({ (x) =>
        x -> UnorderedDataStructureLibrary.getBestStructureForClass(x, querified)
      }).toMap

      querified.actualizeQueries(auxiliaryDataStructures)
    }

### Querification

In the first stage of the process, we take lines like

    stuff.filter(x -> x.age > 50).sum(x -> x.income)

and convert them into something pretty similar to SQL queries--that expression might look like

    SELECT SUM(income) FROM stuff WHERE age > 50

SQL-style queries have a bunch of nice properties. For example, we might want to observe that `list.filter(predicate1).filter(predicate2)` is the same as `list.filter(predicate2).filter(predicate1)`. The process of converting these expressions to specialised `UnorderedQuery` objects (as defined in `UnorderedQuery.scala`) allows this kind of logic to happen.

So that's what the `querify` method does. Here's an, uh, artist's impression of what our Java class looks like after querification:

    class Example
      class Person
        def initialize(age, income)
          @age = age

          @income = income
        end
      end

      def initialize
        @stuff = MagicMultiset.new
      end

      def getAverageIncomeOfOverFifties
        totalIncome = UnorderedQuery(JavaVariable(stuff),Set(WhereClause(x -> x.age > 50)),None,Some(Reduction[0, (x -> x.income), (x, y -> (x + y))]))
        numberOfPeople = UnorderedQuery(JavaVariable(stuff),Set(WhereClause(x,JavaFieldAccess(JavaVariable(x),age),JavaMath(50),false)),None,Some(Reduction[0, (x -> 1), (x, y -> (x + y))]))

        (totalIncome * (numberOfPeople**-1))
      end

      def insertPerson(age, income)
        stuff.insert(age, income)
      end
    end


### Finding auxiliary data structures

Now that we've got all the queries we want our data sets to be able to quickly answer, we need to choose appropriate data structures.

To do this, we need to pay attention to the modification methods which are being called on our data structures, because there are some data structures which you can use only on immutable sets, and some which allow insertion but not removal, and some which allow both.

For example, if we want to keep track of the minimum age of a person in our data structure and we didn't allow removal, we'd just store the minimum age we'd seen so far, and then update it whenever we saw a younger person. Sadly, this doesn't work if we're allowed to remove people: after the youngest person is removed, you need to linear search for the next youngest person. If you need removal in this situation, you'd probably want to be using a heap.

The auxiliary data structures are chosen by the `UnorderedDataStructureLibrary`. Here's how this works:

    helpfulStructures
      .flatMap { _.tryToCreate(query) }
      .filter(_._1.insertionFragment.isDefined || ! requiresInsert)
      .filter(_._1.removalFragment.isDefined || ! requiresRemove)
      .sortBy(_._2)
      .headOption
      .map(_._1)

We start out with the list of all the data structures we know about.

`tryToCreate` tries to initialize the data structure with a given query, and if successful returns data structure and the Big O complexity which the query will take using that structure. If the query is wildly inappropriate for the data structure, like trying to use a frequency histogram to store an average, it returns None.

Next, if we need to be able to insert, we filter out the data structures which can't deal with insertion. Same with removal.

Next, we sort by the Big O complexity which the data structure needs to run a given query.

Finally, we take the head of this list and throw away the time complexity.

In our running example, we end up with two separate `MonoidMemoizer`s, one to store the running total of income for people older than 50 and one to store the running count of people older than 50.


### Actualizing queries

Now, we need to turn our queries into actual Java method calls. This basically works by mapping over the Java class and looking for queries, then turning each query into the right code.

We also make, if required, an insertion and/or removal method for each multiset we're dealing with. This just involves concatenating all of the auxiliary data structures' insertion and removal code into one method. The logic for this lives in the `MagicMultiset` class.

And that's the optimization done. Here's the final Ruby output:

    class Example
      class Person
        def initialize(age, income)
          @age = age

          @income = income
        end
      end

      def initialize
        @stuff = MagicMultiset.new
      end

      def getAverageIncomeOfOverFifties
        totalIncome = @foo
        numberOfPeople = @bar

        (totalIncome * (numberOfPeople**-1))
      end

      def insertPerson(age, income)
        insertInto_stuff(age, income)
      end

      def insertInto_stuff(age, income)
        @foo = (@foo + item.income)
        @bar = (@bar + 1)

        @stuff.insert(age, income)
      end
    end

## Where this project is, and where it is going

I plan to work on this pretty much all the time I'm not at my real job or sleeping until my presentation at Scala By The Bay on Saturday. So the level of progress now is hopefully pretty different to where it will be on Saturday.

I have a lot of work left to do.

- One of the components in this project is a computer algebra system (you can see it in the `cas` package). I've put a lot of time into that component, and probably need to put a lot more in. In particular, its solving abilities are currently embarrasingly limited.
- There should be an `OrderedQuery` class as well as an `UnorderedQuery` class, to make my system able to answer questions about arrays and stacks and queues etc.
- I want to write more implementations of "useful data structures".
- I want to make my outputter target more languages.
- Instead of choosing auxiliary data structures based on Big O complexity, this system should choose them based on a numerical estimate of query time.
- Automatic benchmarking! Automatic unit testing!
- I want to put automatic dynamic programming in here too

The system is working with the shape I intend it to. It's at least a proof of concept.

Ruining the coding interview will be hard, but I think I have a shot.

## Contributing

I'm interested in getting people to help me with this. Things are currently changing too fast for me to reasonably expect anyone else to be able to do anything. Eventually I'll come up with a way to make it easier for people to help me.

## Server usage

    $ curl --request POST --header 'Content-Type: application/json; charset=UTF-8'  --data '{"contents": "class Blah {}"}' localhost:8888/compile
    class Blah
    end

## Information on the code

    $ find . -name '*.scala' | xargs wc -l | sort -r
      3121 total
       327 ./src/main/scala/cas/MathExp.scala
       262 ./src/main/scala/cas/BinaryOperatorApplications.scala
       157 ./src/main/scala/java_transpiler/JavaExpression.scala
       143 ./src/main/scala/java_transpiler/queries/UnorderedQuery.scala
       137 ./src/main/scala/ast_renderers/RubyOutputter.scala
       132 ./src/test/scala/cas/ExpressionTests.scala
       125 ./src/main/scala/java_transpiler/JavaClass.scala
       108 ./src/main/scala/java_transpiler/queries/WhereClause.scala
       102 ./src/main/scala/cas/CasBinaryOperator.scala
        91 ./src/main/scala/java_transpiler/JavaStatement.scala
        89 ./src/main/scala/java_transpiler/JavaMethodDeclaration.scala
        83 ./src/test/scala/cas/GenericExpressionTests.scala
        83 ./src/main/scala/java_transpiler/queries/Reduction.scala
        80 ./src/main/scala/java_transpiler/JavaType.scala
        73 ./src/main/scala/big_o/BigO.scala
        68 ./src/main/scala/useful_data_structures/UsefulUnorderedDataStructure.scala
        65 ./src/main/scala/data_structure_handlers/TestLoadingJavaDataStructure.scala
        63 ./src/main/scala/java_transpiler/AstModifier.scala
        62 ./src/main/scala/useful_data_structures/data_structure_library/GroupMemoizerFactory.scala
        56 ./src/main/scala/java_transpiler/queries/LimitByClause.scala
        56 ./src/main/scala/java_transpiler/MagicMultiset.scala
        53 ./src/main/scala/java_parser/JavaParserWrapper.scala
        50 ./src/main/scala/useful_data_structures/data_structure_library/MonoidMemoizerFactory.scala
        49 ./src/main/scala/useful_data_structures/data_structure_library/PriorityQueueFactory.scala
        42 ./src/main/scala/data_structure_handlers/GenericDataStructureForMultiset.scala
        41 ./src/main/scala/finatra_server/CompilationController.scala
        37 ./src/main/scala/java_transpiler/JavaMathHelper.scala
        37 ./src/main/scala/cas/DodgierCasFunction.scala
        37 ./src/main/scala/Optimizer.scala
        36 ./src/main/scala/java_parser/JavaToAst.scala
        32 ./src/main/scala/cas/Multiset.scala
        31 ./src/main/scala/useful_data_structures/UnorderedDataStructureLibrary.scala
        31 ./src/main/scala/java_transpiler/JavaExpressionOrQuery.scala
        29 ./src/main/scala/useful_data_structures/UsefulDataStructureHelper.scala
        28 ./src/main/scala/java_transpiler/JavaFieldDeclaration.scala
        26 ./src/main/scala/java_transpiler/QueryActualizer.scala
        25 ./src/test/scala/java_transpiler/JavaMethodParsingSpecs.scala
        23 ./src/main/scala/finatra_server/HelloWorldServer.scala
        20 ./src/main/scala/data_structure_handlers/ChimeraMultisetClass.scala
        19 ./src/main/scala/data_structure_handlers/MutatingMethodImplementation.scala
        18 ./src/main/scala/helpers/VariableNameGenerator.scala
        18 ./src/main/scala/helpers/UnorderedPair.scala
        17 ./src/main/scala/external_interfaces/ExternalInterfaces.scala
        12 ./src/main/scala/java_transpiler/queries/OrderByClause.scala
        12 ./src/main/scala/java_transpiler/AstBuilder.scala
         7 ./src/main/scala/java_transpiler/queries/JavaContext.scala
         7 ./src/main/scala/java_transpiler/JavaBinaryOperator.scala
         7 ./src/main/scala/finatra_server/HiRequest.scala
         5 ./src/main/scala/java_transpiler/VariableScopeDetails.scala
         5 ./src/main/scala/java_transpiler/InternalTypeError.scala
         5 ./src/main/scala/cas/Name.scala
