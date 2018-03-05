package Week2


/**
  * 2. Purely Functional Sets.
  */
object Exercises  {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (value) => value == elem


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = (value) => contains(s,value) || contains(t,value)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = (value) => contains(s,value) && contains(t,value)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = (value) => contains(s,value) && !contains(t,value)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = (value) => contains(s,value) && p(value)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (contains(s,a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    //def not(f: Int => Boolean) = (value: Int) => !f(value)
    //!forall(s,not(p))
    !forall(s,(value) => !p(value))
  }


  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = (value) => exists(s,(x) => value == f(x))

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }

  def main(args: Array[String]): Unit = {


    assert(contains(x => true, 100))

    trait TestSets {
      val s1 = singletonSet(1)
      val s2 = singletonSet(2)
      val s3 = singletonSet(3)
    }

    new TestSets {
        assert(contains(s1, 1), "Singleton")
    }

    // union tests
    new TestSets {
        val s = union(s1, s2)
        assert(contains(s, 1), "Union 1")
        assert(contains(s, 2), "Union 2")
        assert(!contains(s, 3), "Union 3")
        //printSet(s)
    }

    // intersection tests
    new TestSets {
      val s = union(s1, s2) // 1 2
      val s4 = union(s1, s3) // 1 3
      val intersection = intersect(s,s4)// 1
      assert(contains(intersection, 1), "Intersection 1")
      assert(!contains(intersection, 3), "Intersection 2")
      assert(!contains(intersection, 2), "Intersection 3")
      //printSet(intersection)
    }

    // diff tests
    new TestSets {
      val s = union(s1, s2) // 1 2
      val s4 = union(s1, s3) // 1 3
      val s5 = diff(s,s4)// 2
      assert(contains(s5, 2), "Diff 1")
      assert(!contains(s5, 1), "Diff 2")
      assert(!contains(s5, 3), "Diff 3")
      //printSet(diff)
    }

    // filter tests
    new TestSets {
      val s = union(s1, s2) // 1 2
      val s4 = union(s, s3) // 1 2 3
      val filtered = filter(s4, value => value > 1) // 2 3
      assert(contains(filtered, 2), "Filter 1")
      assert(contains(filtered, 3), "Filter 2")
      assert(!contains(filtered, 1), "Filter 3")
      //printSet(filtered)
    }

    // forAll / exists
    new TestSets {
      val setMinus1000 = singletonSet(-1000)
      val setPlus1000 = singletonSet(1000)
      val setPlus500 = singletonSet(500)
      val setZero = singletonSet(0)
      val fullSet = union(union(union(setMinus1000,setPlus1000),setZero),setPlus500)
      printSet(fullSet)
      assert(forall(fullSet,(value) => value != 250)) // should not contain 250
      assert(!forall(fullSet,(value) => value != 0)) // should contain 0
      assert(forall(fullSet,(value) => value % 500 == 0)) // should be divisible by 500
      assert(!forall(fullSet,(value) => value > 0)) // not every value is positive
      assert(exists(fullSet,(value) => value > 0)) // there exists negative numbers
      assert(exists(fullSet,(value) => value < 0)) // there exists positive numbers
      assert(!exists(fullSet,(value) => value > 1000))
      val fullSetMapped = map(fullSet,value => value/10)
      printSet(fullSetMapped)
      assert(contains(fullSetMapped,-100))
      assert(contains(fullSetMapped,100))
      assert(contains(fullSetMapped,50))
    }
  }
}