def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList(ys)
  }

def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => x *x)

squareList(List(1,2,3))
squareListMap(List(1,2,3))

pack(List("a", "a", "a", "b", "c", "c", "a"))
def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => List(x) :: xs1.takeWhile(v => v == x) ::
    pack(xs1.dropWhile(v => v == x))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))
List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
def encode[T](xs: List[T]): List[(T,Int)] = xs match {
  case Nil => Nil
  case x :: xs1 => (x, xs1.takeWhile(v => v == x).length + 1) ::
    encode(xs1.dropWhile(v => v == x))
}

def encodeMap[T](xs: List[T]): List[(T,Int)] = {
  pack(xs) map (x => (x(0), x.length))
}

encodeMap(List("a", "a", "a", "b", "c", "c", "a"))