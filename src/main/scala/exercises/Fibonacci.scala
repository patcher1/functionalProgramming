package exercises

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int):Int = {
    @tailrec
    def loop(n1: Int, n2: Int,i: Int):Int={
      if(i>=n){ //The > is that inputs n<1 do not cause "infinite" recursion
        n1
      }else{
        loop(n2,n1 + n2,i+1)
      }
    }
    loop(0,1,0)
  }

  def fibAlternative(n: Int):Int = {
    @tailrec
    def go (n1: Int, n2: Int, n:Int):Int = {
      if (n == 0) n1
      else {
        go(n2,n1+n2,n-1)
      }
    }
    go(0,1,n)
  }

  def main(args: Array[String]):Unit = {
    println(fib(0))
    println(fib(1))
    println(fib(2))
    println(fib(3))
    print(fib(4))
    println(fib(20))

    println("alternative")

    println(fibAlternative(0))
    println(fibAlternative(1))
    println(fibAlternative(2))
    println(fibAlternative(3))
    print(fibAlternative(4))
    println(fibAlternative(20))
  }

}
