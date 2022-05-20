package illustrations

object ReferentialTransparency {

  //An expression is referentially transparent if it can be
  //replaced by its corresponding value (evaluation) without
  //changing the behaviour of the program

  //A Function is pure if:
  //- It relates every value a of type A to exactly one output value b of type B
  //  so f: A => B, a -> b
  //  So a pure function is a mathematical function
  //- The expression f(x) is referentially transparent for all expressions
  //  x that are itself referentially transparent.

  def main(args: Array[String]):Unit = {
    //Illustrate referential integrity
    val x = "Hello World"
    var r1 = x.reverse
    var r2 = x.reverse
    println(r1)
    println(r2)

    //We can replace x by the expression referenced by x
    //without affecting the outcome
    //We could in this case even go further and replace "Hello World".reverse
    //by "dlroW olleH" and it would not change the outcome
    //If r1, r2 would appear in some other place we could also replace them, it would not affect
    //the outcome
    r1 = "Hello World".reverse
    r2 = "Hello World".reverse
    println(r1)
    println(r2)

    //Illustrate referential opacity
    val a = new StringBuilder("Hello")
    val b = a.append(" World")

    var s1 = b.toString()
    var s2 = b.toString()

    println(s1)
    println(s2)

    //Replace by referenced expression
    s1 = a.append(" World").toString() //Hello World World
    s2 = a.append(" World").toString() //Hello World World World

    println(s1)
    println(s2)

    //append(str) is not a pure function and the expressions b.toString not referentially transparent!
    //append affects some state inside StringBuilder
    //With referential integrity we can use substitutions (Substitution model) to understand
    //and verify code. The effects are only local and we do not need to track state updates somewhere else.
  }


}
