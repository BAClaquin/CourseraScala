
def merge(xs: List[Int], ys: List[Int]): List[Int] =
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xt, y :: yt) => {
      if(x < y) x :: merge(xt, ys)
      else y :: merge(xs, yt)
    }
  }

merge(List(1, 3, 4, 5), List(2, 4, 6))