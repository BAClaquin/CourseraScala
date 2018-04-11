// just a bunch of potentially useful stuff

// fold
val numbers = List(5, 4, 8, 6, 2)
numbers.fold(0) { (z, i) =>
  z + i
}

numbers.foldRight(0)((a,b) => a + b)
numbers.foldRight(1)((a,b) => a * b)

val numbers2 = List(10,5)
numbers2.foldRight(1)((a,b) => a / b)
numbers2.foldLeft(1)((a,b) => a / b)
numbers2.fold(1)((a,b) => a / b)

val fruits = List("apple","banana")
fruits.fold("")((a,b) => a+b)


// map
val randomMap = Map("key1"->"value1","key2"->"value2")
val invertedMap =
  for ((key, value) <- randomMap) yield value -> key

   // multi layer map
val randomMapII = Map("key1" -> List("value1.1","value1.2"), "key2"->List("value2"))
val invertedMapII =
  for ((key, values) <- randomMapII; value <- values) yield value -> key