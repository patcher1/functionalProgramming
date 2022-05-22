package exercises

object HigherOrderFunctions {

  //Exercise 2.3
  //Only one possible implementation
  //Returns a function g such that
  // g: A => (B => C)
  // g(a) = f(a,.) := f_a(.)
  // g(a)(b) = f(a,b)
  //Partial application with undetermined a
  def curry[A,B,C] (f:(A,B) => C): A => (B => C) = {
    a => (b => f(a,b))
  }

  //Exercise 2.4
  //For g: (a:A) => f(a,.) this returns f
  def uncurry[A,B,C](f:A => (B => C)): (A,B) => C = {
    (a,b) => f(a)(b)
  }


  //Exercise 2.5
  //Returns the function
  // h: A => C
  // h(a) = g(f(a))
  def compose[A,B,C](f: A => B, g:B => C): A => C = {
    a => g(f(a))
  }

  def main(args: Array[String]):Unit = {
    val f = (a:Int,b:Int) => a + b //f is of type (Int,Int) => Int
    val fcurry = curry(f)
    val g = fcurry(2)
    val h = fcurry(3)
    println(g(1))
    println(h(1))

    val originalF = uncurry(fcurry)
    println(originalF(1,1))

    val f1 = (a:Int) => 2*a
    val f2 = (b:Int) => 4*b
    val comp = compose(f1,f2)
    println(comp(1))
  }
}
