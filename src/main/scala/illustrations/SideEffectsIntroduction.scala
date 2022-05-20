package illustrations

object SideEffectsIntroduction {

  case class CreditCard(){
    def charge(amount: Double):Unit = {

    }
  }
  class Coffee(){
    val price = 2
  }

  class CafeWithSideEffects {

    //Non pure function, invokes cc.charge as side effect
    def buyCoffee(cc: CreditCard):Coffee = {
      val cup = new Coffee()
      //would be hard to test if some real service is behind, would need special
      //techniques to mock the CreditCard
      cc.charge(cup.price)
      cup
    }

  }

  trait Payment{
    def charge(amount: Double,creditCard: CreditCard):Unit
  }
  class ConcretePayment extends Payment{
    override def charge(amount: Double,creditCard: CreditCard): Unit = {
      //do something
    }
  }
  class ImprovedCafe {
    //Above suggested that CreditCard itself should not know about any payment processing
    //The trait Payment is easily mockable, the function more testable
    //Still not a pure function, p.charge is still a side effect
    //Imagine we just want to test if buyCoffee creates equal to the price of a coffee
    //We would have to mock a Payment object and inspect it later -> overkill
    //Another problem: If we want to buy multiple coffees, this will contact payment
    //services multiple times! We could attempt to create a BatchPayment class
    //that collects charges...but horribly complicated for such a simple thing
    def buyCoffee(cc: CreditCard, p: Payment):Coffee = {
      val cup = new Coffee()
      p.charge(cup.price,cc)
      cup
    }
  }

  case class Charge(cc: CreditCard, amount: Double ){

    def combine(other: Charge): Charge ={
      if(cc == other.cc)
        Charge(cc,amount + other.amount)
      else {
        //Throwing exceptions is not very functional
        throw new Exception("Can not combine charges to different credit cards!")
      }
    }

  }
  class FunctionalCafe {
    //Separation of concerns: We separated the creation of a charge from the interpreting / processing
    //of that charge
    //Pure Function, performs no side effects
    def buyCoffee(cc: CreditCard):(Coffee,Charge) = {
      val cup = new Coffee()
      (cup,Charge(cc,cup.price))
    }

    //The pure nature of buyCoffee makes it easy to use for buyCoffees
    //We can easily combine the charges into a single one i.e.
    //in the old solutions this would not have been possible without
    //effectively charging the credit cards in every step
    def buyCoffees(n: Int,cc: CreditCard):(List[Coffee],Charge) = {
      val purchases = List.fill(n)(buyCoffee(cc))
      val (coffees,charges) = purchases.unzip
      (coffees, charges.reduce((charge1,charge2) => Charge(cc,charge1.amount + charge2.amount)))
    }

    //Combine charges of the same credit card into a single one
    def coalesce(charges: List[Charge]):List[Charge] = {
      //Inside reduce: The first _ refers to the first Charge object, the second _ to the second one.
      charges.groupBy(_.cc).values.map(_.reduce(_.combine(_))).toList
    }
  }
  def main(args: Array[String]): Unit = {

  }

}
