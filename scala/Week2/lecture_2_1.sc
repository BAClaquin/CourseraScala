
// sum of f(integers) between a and b
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a == b) acc
    else loop(a + 1, acc + f(a)) // tail recursive
  }
  loop(a, f(b))
}

sum(x => x * x)(1,4) //30
sum(x => x * x * x)(1,100)