package exercises

import exercises.LinkedList.{addLists, append, appendWithFold, applyUsingFoldRight, concat, drop, dropWhile, dropWhileCurried, foldLeft, foldRight, foldRightUsingFoldLeft, init, mapUsingFold, productWithFold, reverse, setHead, sum, sumWithFold, zipWith}

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

  def applyUsingFoldRight[A](list: A*):LinkedList[A]={
    list.foldRight(Nil: LinkedList[A])(Cons(_,_))
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

//  Exercise 3.10
//  Tail-recursive, no danger of stackoverflow
  @tailrec
  def foldLeft[A,B](as: LinkedList[A], z: B)(f: (B, A) => B): B = {
    as match {
      //At the end of the list, this z will be z = f(f(...f(z,h_1),h_2),...,h_n)
      //i.e. f will be applied from left to right with an initial left operand = z
      case Nil => z
      //Pass f(z,h) as the "new" z then for example after one recursion:
      //f(f(z,h1),h2) = z
      case Cons(h,t) => foldLeft(t,f(z,h))(f)
    }
  }

  //exercise 3.12
  def reverse[A](as: LinkedList[A]):LinkedList[A]={
    foldLeft(as,Nil:LinkedList[A])((t,h) => Cons(h,t))
  }

  //exercise 3.13, This avoids stackoverflows since foldLeft is tail recursive
  def foldRightUsingFoldLeft[A,B](as: LinkedList[A], z: B)(f: (A,B) => B): B = {
    foldLeft(reverse(as),z)((l,r) => f(r,l))
  }

  //exercise 3.14
  def appendWithFold[A](l: LinkedList[A], r: LinkedList[A]):LinkedList[A]={
    foldRight(l,r)((h,t) => Cons(h,t))
  }

  //exercise 3.15, "official" solution uses foldRight but foldLeft also works well
  //and is stack-safe
  def concat[A](listOfLists: LinkedList[LinkedList[A]]): LinkedList[A]={
    foldLeft(listOfLists,Nil:LinkedList[A])((t,h)=> append(t,h))
  }

//  exercise 3.18
  def mapUsingFold[A,B](as: LinkedList[A])(f: A => B): LinkedList[B] = {
    foldRightUsingFoldLeft(as,Nil:LinkedList[B])((h,t) => Cons(f(h),t))
  }

  //More usual map implementation with local mutable ListBuffer
  def map[A,B](l: LinkedList[A])(f: A => B): LinkedList[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    @tailrec
    def go(l: LinkedList[A]): Unit = l match {
      case Nil => () //the only value of type unit
      case Cons(h,t) => buf += f(h)
                        go(t)
    }
    go(l)

    //converting to LinkedList
    LinkedList(buf.toList: _*)
  }

  //exercise 3.19
  def filter[A](l: LinkedList[A])(predicate: A => Boolean): LinkedList[A]={
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(l: LinkedList[A]):Unit = l match {
      case Nil => ()
      case Cons(h,t) => if(predicate(h)) buf+= h; go(t)
    }
    go(l)
    LinkedList(buf.toList: _*)
  }

  //exercise 3.20
  def flatMap[A,B](as: LinkedList[A])(f: A => LinkedList[B]):LinkedList[B] = {
    concat(map(as)(f))
  }

  //exercise 3.21
  def filterWithFlatMap[A](l: LinkedList[A])(predicate: A => Boolean): LinkedList[A]={
    flatMap(l)(e => if(predicate(e)) Cons(e,Nil) else Nil)
  }

  //exercise 3.22 (stack-safe)
  def addLists (l1: LinkedList[Int], l2: LinkedList[Int]):LinkedList[Int] = {
    val buf = new collection.mutable.ListBuffer[Int]
    @tailrec
    def go(l1: LinkedList[Int],l2: LinkedList[Int]): Unit = (l1,l2) match {
      case (Nil,_) => () //the only value of type unit
      case (_,Nil) => ()
      case (Cons(h1,t1),Cons(h2,t2)) => buf += h1 + h2
                                        go(t1,t2)
    }
    go(l1,l2)

    //converting to LinkedList
    LinkedList(buf.toList: _*)
  }

  //exercise 3.23 (stack-safe)
  def zipWith[A,B,C](l1: LinkedList[A],l2: LinkedList[B])(op: (A,B) => C): LinkedList[C] = {
    val buf = new collection.mutable.ListBuffer[C]
    @tailrec
    def go[D,E](l1: LinkedList[D],l2: LinkedList[E])(op: (D,E) => C): Unit = (l1,l2) match {
      case (Nil,_) => ()
      case (_,Nil) => ()
      case (Cons(h1,t1),Cons(h2,t2)) => buf += op(h1,h2)
                                        go(t1,t2)(op)
    }
    go(l1,l2)(op)

    //converting to LinkedList
    LinkedList(buf.toList: _*)
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
    val anotherList = LinkedList(5,6,7,8)

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
    println(append(list,anotherList))

    println("List without last------------------------")
    println(init(list))
    println(init(init(list)))

    println("Sum & Product with foldRight------------------------")
    println(sumWithFold(list))
    val doubleList = LinkedList(1.0,2.0,3.0,4.0)
    println(productWithFold(doubleList))

//    Below will not work the return is non-local
//    it will return out of this function, not the anonymous one
//    val doubleWithZero = LinkedList(1.0,0,3.0)
//    val prod = LinkedList.foldRight(doubleWithZero,1.0)((x,y) => {
//      if(x == 0){
//        return 0
//      }else{
//        x * y
//      }
//    })
//    println("product: ")
//    println(prod)
//    Exercise 3.7: It is not possible to stop immediately when encountering a zero
//    before calling the function f in foldRight we go through the whole list.

//    Exercise 3.8
    println("Call foldRight with the Cons constructor------------------------")
    //We get back to original list
    val resultList = LinkedList.foldRight(LinkedList(1,2,3), Nil:LinkedList[Int])(Cons(_,_))
    println(resultList)

//  This implies we can write LinkedList.apply using a foldRight
    println(applyUsingFoldRight(1,2,3,4))

    println("foldLeft------------------------")
    //The + operator is associative and 0 is the neutral element of +
    //so the foldLeft: (((0+1)+2)+3)+4 = 1+(2+(3+(4+0)))
    //is equal to the foldRight in this case
    println(s"Sum with foldRight: ${foldRight(list,0)(_+_)}")
    println(s"Sum with foldLeft: ${foldLeft(list,0)(_+_)}")

    //exercise 3.11
    println("exercise 3.11------------------------")
    println(s"Sum with foldLeft: ${foldLeft(list,0)(_+_)}")
    println(s"Product with foldLeft: ${foldLeft(doubleList,1.0)(_*_)}")
    println(s"Count with foldLeft: ${foldLeft(list,0)((x,_) => x+1)}")



    println("Reverse List--------------------")
    println(reverse(list))

    println("FoldRight using foldLeft--------------------")
    println(s"Sum with foldRight: ${foldRight(list,0)(_+_)}")
    println(s"Sum with foldRightUsingFoldLeft: ${foldRightUsingFoldLeft(list,0)(_+_)}")

    println("Append with foldLeft-----------------")
    println(appendWithFold(list,anotherList))

    println("Concat-----------------")
    val listOfLists = LinkedList(list,anotherList,LinkedList(1,1,1))
    println(concat(listOfLists))

    println("Map------------------------------")
    val doubledList = mapUsingFold(list)(x => x*2)
    println(s"map using foldRight: $doubledList")
    println(s"map using buffer: ${LinkedList.map(list)(_*2)}")

    println("Filter------------------------------")
    println(s"Numbers > 2: ${LinkedList.filter(list)(_>2)}")
    println(s"Numbers < 3: ${LinkedList.filter(list)(_<3)}")

    println("FlatMap------------------------------")
    println(s"Duplicate the list: ${LinkedList.flatMap(list)(n => LinkedList(n,n))}")

    println("Filter with flatMap------------------------------")
    println(s"Numbers > 2: ${LinkedList.filter(list)(_>2)}")
    println(s"Numbers < 3: ${LinkedList.filter(list)(_<3)}")

    println("Adding two lists---------------------------------")
    val l1 = LinkedList(1,2,3)
    val l2 = LinkedList(1,2,3)
    val l3 = LinkedList(1,2)
    println(s"Adding two same sized lists: ${addLists(l1,l2)}")
    println(s"Adding a smaller and a larger list: ${addLists(l3,l1)}")
    println(s"Adding a larger and a smaller list: ${addLists(l1,l3)}")

    println("Using zipWith to: ---------------------------------")
    println(s"- Add two lists: ${zipWith(l1,l2)(_+_)}")
    println(s"- Multiply two lists ${zipWith(l1,l2)(_*_)}")
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

