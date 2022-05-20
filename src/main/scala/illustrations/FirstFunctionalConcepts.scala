package illustrations

import scala.annotation.tailrec

object FirstFunctionalConcepts {

  //Should try to use tail recursive functions, will be compiled to an iterative
  //loop. Better performance & no stack overflows.
  def factorialNonTailRecursive(n: Int):Int = {
    if (n <= 1)
      1
    else
      n*factorialNonTailRecursive(n-1)
  }

  /**
   * Calculates the factorial n!.
   * @param n
   * @return The factorial n! or 1 if n <= 1
   */
  def factorial(n: Int):Int = {
    @tailrec
    def loop(n: Int, product: Int):Int = {
      if (n <= 1)
        product
      else
        loop(n-1,product*n)
    }
    loop(n,1)
  }

  def main(args: Array[String]):Unit = {
    println(factorialNonTailRecursive(5))
    println(factorial(5))
    println(factorial(-4))
    println(factorial(0))
  }
}
