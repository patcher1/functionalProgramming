package illustrations

import scala.annotation.tailrec

//Functional Data Structure: A data structure operated on using only pure functions.
//Pure functions can not have side effects i.e. change data in place, thus
//functional data structures are immutable. Typical operations like adding
//elements do not modify a data structure, they create a new one.

//+A: covariant, LinkedList[B] is a subtype of LinkedList[A] if
//B is a subtype of A
sealed trait LinkedList[+A] {
   def buildStringRepresentation(str: String):String
}

//Nothing is a subtype of all types => List[Nothing] is a subtype of all List[A]
//This represents an empty list
case object Nil extends LinkedList[Nothing] {
  override def buildStringRepresentation(str: String): String = {
    ""
  }
}

case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A] {
  override def toString: String = {
    buildStringRepresentation("LinkedList(")+")"
  }
  def buildStringRepresentation(str: String): String = {
     if (tail != Nil) tail.buildStringRepresentation(str+head.toString+", ")
     else str+head.toString
  }
}

object LinkedList {
  def sum(ints: LinkedList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  def product(ds: LinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def main (args: Array[String]):Unit = {
    val intList = LinkedList(1,2,5,3,2,1) //calls apply
    println(intList)
  }
}

