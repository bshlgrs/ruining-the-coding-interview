# Ruining the coding interview

(This code is very much a work in progress. You can get a simple demo from running it with `sbt run`: by default this optimizes the code in `example-apis/example.java`.)

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

So my project is to make optimizing compilers at a higher level than that. This is obviously a highly ambitious project.

For this presentation I’m just going to be talking about the part of this I’ve implemented. Programmers should be able to use high level data structures like multisets and have a compiler decide what data structures the multisets should be using under the hood. I’m trying to make that happen.

Here’s the only problem. My adventures into automatically generating data structures lead me to a wide variety of complicated data structures, which I find fascinating but I feel very little desire to actually implement. So while I’m interested in the question of what data structures you should use, I’m not actually interested in doing all the hard work which I’d need to do to actually generate usable code. In real life, you’re not allowed to just say “and now you use a heap here”, but in coding interviews you are.

So for now, I’m not trying to improve programming. I just want to ruin the coding interview.

## Data structure optimizer

Okay, so how are we going to do this?

(I’ve ended up with this project compiling Java to Ruby. You might point out that this is kind of weird for a Scala project, and you’d be right. One reason I’m currently going Java to Ruby is that Java has explicit types, which makes it easier for me to reason about, and Ruby doesn’t, which makes it easier for me to output it.)

Here’s an example of a simple Java class. It stores a list of people. You can add people to it, and whenever you want you can ask it to tell you the average income of people who are older than fifty:

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

to be continuted
