
// sum of f(integers) between a and b
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) 0
    else loop(a + 1, acc + f(a)) // tail recursive
  }
  loop(a, b)
}

sum(x => x * x)(3, 5) //30
sum(x => x * x * x)(1, 100)