package exercises

import exercises.LinkedList.{append, drop, dropWhile, dropWhileCurried, init, productWithFold, setHead, sum, sumWithFold}

import scala.annotation.tailrec

//Functional Data Structure: A data structure operated on using only pure functions.
//Pure functions can not have side effects i.e. change data in place, thus
//functional data structures are immutable. Typical operations like adding
//elements do not modify a data structure, they create a new one.

//+A: covariant, LinkedList[B] is a subtype of LinkedList[A] if
//B is a subtype of A
sealed trait LinkedList[+A] {
//   def buildStringRepresentation(str: String):String

  //EXERCISE 3.2
  def tail: LinkedList[A]
}

//Nothing is a subtype of all types => List[Nothing] is a subtype of all List[A]
//This represents an empty list
case object Nil extends LinkedList[Nothing] {
//  override def buildStringRepresentation(str: String): String = {
//    ""
//  }

  //EXERCISE 3.2
  override def tail: LinkedList[Nothing] = Nil
}

case class Cons[+A](head: A, tail: LinkedList[A]) extends LinkedList[A] {

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

  def sumWithFold(ints: LinkedList[Int]):Int = {
    foldRight(ints,0)((x,y) => x+y)
  }

  def productWithFold(ds: LinkedList[Double]): Double = {
    foldRight(ds,1.0)((x,y) => x*y)
  }

  def product(ds: LinkedList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  //f is a binary operator getting applied from right to left
  //The evaluation goes as follows:
  //1) f(h1,foldRight(list,special)(f))
  //2) f(h1,f(h2,foldRight(list,special)(f)))
  //3) f(h1,f(h2,f(h3,foldRight(list,special)(f)))) ...
  //So the evaluation happens from "inner" function to "outer" i.e. from
  //right to left starting with f(h_n,initial) if h_n is the last element of the list.
  def foldRight[A,B](list: LinkedList[A],initial: B)(f:(A,B) => B):B={
    list match {
      case Nil => initial
      case Cons(h,t) => f(h,foldRight(t, initial)(f))
    }
  }

  //A* is an ArraySeq with an underlying array

  def apply[A](as: A*): LinkedList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  //EXERCISE 3.3
  def setHead[A](h: A, l: LinkedList[A]): LinkedList[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  //EXERCISE 3.4
  @tailrec
  def drop[A](l: LinkedList[A], n: Int):LinkedList[A] = {
    if(n >= 0) drop(l.tail,n-1)
    else l
  }

  //EXERCISE 3.5
  @tailrec
  def dropWhile[A](l: LinkedList[A], f: A => Boolean): LinkedList[A] = {
    l match {
      case Cons(h,t) if f(h) =>  dropWhile(t,f) // pattern guard, more elegant than using if inside
      case _ => l
    }
  }
  //EXERCISE 3.5
  def dropWhileCurried[A](l: LinkedList[A])(f: A => Boolean): LinkedList[A] = {
    l match {
      case Cons(h,t) if f(h) =>  dropWhile(t,f) // pattern guard, more elegant than using if inside
      case _ => l
    }
  }


  //This function is O(n) where n = l1.length, the last Con of l1 just points via its tail
  //to l2.
  def append[A] (l1: LinkedList[A],l2: LinkedList[A]): LinkedList[A] = {
    l1 match {
      //until Nil is reached, we will just append the elements of l1 to the new Cons
      case Cons(h,t) => Cons(h,append(t,l2))
      case Nil => l2
    }
  }
  //Exercise 3.6
  //In the real world we would use an internal mutable list to implement this,
  //not tail recursive so stackoverflows happen for large lists.
  def init[A](l: LinkedList[A]): LinkedList[A] = {
    l match {
      case Cons(_,Nil) => Nil //The order is important here for matching!
      case Nil => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  //EXERCISE 3.9
  def length[A](l: LinkedList[A]):Int = {
    //In the first evaluation count = initial
    //Remember: The right element will always be the result of previous
    //calculations
    foldRight(l,0)((_,count) => count + 1)
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
    val headRemoved = list.tail

    println("Set Head & Drop------------------------")
    println(headRemoved)
    println(setHead(5,list))
    println(setHead(1,Nil))
    println(drop(list,2))
    println(drop(list,4))
    println(drop(list,5))

    println("Drop While------------------------")
    println(dropWhile(list,(n:Int) => n < 4))
    println(dropWhile(list,(n:Int) => true))

    println("Drop While: Curried------------------------")
    //In the curried version we do not have to pass the type of n
    //it is inferred from the type of the list
    println(dropWhileCurried(list)(n => n < 4))

    println("Append Lists------------------------")
    val anotherList = LinkedList(5,6,7,8)
    println(append(list,anotherList))

    println("List without last------------------------")
    println(init(list))
    println(init(init(list)))

    println("Sum & Product with foldRight------------------------")
    println(sumWithFold(list))
    val doubleList = LinkedList(1.0,2.0,3.0,4.0)
    println(productWithFold(doubleList))
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

