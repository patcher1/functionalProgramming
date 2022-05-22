package exercises

import scala.annotation.tailrec

//Exercise 2.2
object IsSorted {
  def isSorted[A](as: Array[A],ordered: (A,A) => Boolean):Boolean = {
    @tailrec
    def loop(i: Int):Boolean={
      if(i == as.length-1) true
      else if (ordered(as(i),as(i-1))) false
      else loop(i+1)
    }
    loop(1)
  }


  def main(args: Array[String]):Unit = {
    val orderedArray = Array(1,2,3,4,5)
    val unorderedArray = Array(1,4,3,2,5)
    println(isSorted(orderedArray,(n:Int,m:Int) => n < m))
    println(isSorted(unorderedArray,(n:Int,m:Int) => n < m))

  }
}
