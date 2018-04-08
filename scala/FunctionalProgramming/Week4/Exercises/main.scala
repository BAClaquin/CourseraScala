package FunctionalProgramming.Week4.Exercises
import Huffman._

object Main extends App{
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

    new TestTrees {
      assert(weight(t1) == 5)
    }


    new TestTrees {
      assert(chars(t2) == List('a','b','d'))
    }


    assert(string2Chars("hello, world") == List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))



    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) == List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))

    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    //println(combine(leaflist))
    //println(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    assert(combine(leaflist) == List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))


    new TestTrees {
      println(t1)
      println(encode(t1)("ab".toList))
      println(decode(t1, encode(t1)("ab".toList)))
      assert(decode(t1, encode(t1)("ab".toList)) == "ab".toList)
    }
}
