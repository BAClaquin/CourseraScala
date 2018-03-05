// Currying
// (Int => Int) => ((Int,Int) => Int)
def sum(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 0 else f(a) + sum(f)(a + 1,b) // not tail-recursive then ???

def product(f: Int => Int)(a: Int, b: Int): Int =
  if (a > b) 1 else f(a) * product(f)(a + 1, b)

// using product
def factorial(n: Int): Int = product(x => x)(1, n)

// quite like a mapreduce
def generalFunction(inital: Int, g: (Int, Int) => Int)(f:Int => Int)(a: Int, b: Int): Int =
  if (a > b) inital else g(f(a), generalFunction(inital,g)(f)(a + 1, b))

// using a general funciton
def generalFactorial(n: Int): Int =
  generalFunction(1, (x, y) => x * y)(x => x)(1, n)

sum(x => x * x)(3, 5)
sum(x => x * x)(3, 500)

product(x => x)(1, 4)
factorial(4)
generalFactorial(4)

