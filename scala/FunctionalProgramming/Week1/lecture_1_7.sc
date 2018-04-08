/**
  * Tail-recursive factorial
  */
object Lecture_1_7 {


  def factorial(n: Integer): Integer =
    if (n == 0) 1 else n * factorial(n - 1)


  def tailRecursiveFactorial(n: Integer): Integer = {

    def loop(acc: Integer, n: Integer): Integer =
      if (n == 0) acc
      // tail recursion : function calls itself as it' last action
      else loop(n*acc, n-1)

    loop(1,n)
  }


  factorial(4)
  tailRecursiveFactorial(4)
}


