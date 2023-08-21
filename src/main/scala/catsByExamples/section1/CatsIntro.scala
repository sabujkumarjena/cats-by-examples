package catsByExamples.section1

object CatsIntro {

  // Eq
  val aComparision = 2 == "a string"
  // part 1 - type class import

  import cats.Eq
  // part 2 - import TC instances for the the types you need

  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparision = intEquality.eqv(2, 3) // false
  // val unsafeComparision = intEquality.eqv(2, "A string") // doesn't compile

  // part 4 - use extension methods
  import cats.syntax.eq._

  val anotherTypeSafeComp = 2 === 3 // === is equivalent
  // val invalidComp = 2 === "sabuj"  //doen't compile
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. list
  import cats.instances.list._ // will bring Eq[List[Int]] in scope
  val aListComparision = List(2) === List(1, 2, 3)

  // part 6 -- create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) =>
    c1.price == c2.price
  }

  val compare2ToyCars = ToyCar("Ferrari", 123.44) === ToyCar("Maruti", 123.44)

  /*
  //Most important functionalities are type classes
    import cats.YourTypeClass    //use ur type class API
    import cats.instances.yourType._  //bring implicit TC instances for your supported type in scope
    import cats.syntax.yourTypeClass._ //use extension methods your TC supports

   //Some imports are not self-evident - import all:

   import cats._
   import cats.implicits._
   */

  def main(args: Array[String]): Unit = {
    println(compare2ToyCars)
  }
}
