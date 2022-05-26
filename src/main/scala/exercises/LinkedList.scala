package exercises

import exercises.LinkedList.{drop, setHead, sum, tail}

import scala.annotation.tailrec

//Functional Data Structure: A data structure operated on using only pure functions.
//Pure functions can not have side effects i.e. change data in place, thus
//functional data structures are immutable. Typical operations like adding
//elements do not modify a data structure, they create a new one.

//+A: covariant, LinkedList[B] is a subtype of LinkedList[A] if
//B is a subtype of A
sealed trait LinkedList[+A] {
//   def buildStringRepresentation(str: String):String
}

//Nothing is a subtype of all types => List[Nothing] is a subtype of all List[A]
//This represents an empty list
case object Nil extends LinkedList[Nothing] {
//  override def buildStringRepresentation(str: String): String = {
//    ""
//  }
}

case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
//  override def toString: String = {
//    buildStringRepresentation("LinkedList(")+")"
//  }

}

object LinkedList {


  def sum(ints: LinkedList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  //alternative sum, what it means to "sum" strings
  def sum(strings: LinkedList[String]): String = {
    strings match {
      case Nil => ""
      case Cons(head, tail) => head + sum(tail)
    }
  }

  def product(ds: LinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  //A* is an ArraySeq with an underlying array
  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](ds: LinkedList[A]): LinkedList[A] = {
    ds match {
      case Nil => Nil //choice what to do here
      case Cons(_, tail) => tail
    }
  }

  def setHead[A](h: A, l: LinkedList[A]): LinkedList[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  @tailrec
  def drop[A](l: LinkedList[A], n: Int):LinkedList[A] = {
    if(n >= 0) drop(tail(l),n-1)
    else l
  }
}
object LinkedListFeatures {

  def patternMatchingExercise(): Unit = {
    val x = LinkedList(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y //this will match, most specific
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }

  def modifyingFunctionalDatastructures(): Unit = {
    //Functional datastructures are immutable and in a way persistent
    //Existing references do never get modified
    //Modifying operations create new datastructures while using existing ones (data sharing)
    //for example: Removing the head means we can just return the tail, there is no need
    //for copying. Also no need for pessimistic copying since the tail can also not be changed
    //via existing references.

    val list = LinkedList(1,2,3,4)
    val headRemoved = tail(list)

    println(headRemoved)
    println(setHead(5,list))
    println(setHead(1,Nil))
    println(drop(list,2))
    println(drop(list,4))
    println(drop(list,5))
  }

  def main(args: Array[String]):Unit = {
    //basicFeatures()
    //algebraicFeatures()
    //patternMatchingExercise()
    modifyingFunctionalDatastructures()
  }

  def algebraicFeatures():Unit = {
    val intList = LinkedList(1,2,3,4,5)
    println(sum(intList))

    val stringList = LinkedList("H","E","L","L","O")
    println(sum(stringList))
  }
  def basicFeatures():Unit={
    val intList = LinkedList(1,2,5,3,2,1) //calls apply
    println(intList)
    val stringList = LinkedList("c","b","a")
    val otherStringList = Cons("c",Cons("b",Cons("a",Nil)))
    println(stringList)
    println(otherStringList)
    println(stringList == otherStringList) //equals implementation provided by case class

    val doubleList: LinkedList[Double] = Nil
    println(doubleList)
  }

}

