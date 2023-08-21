package catsByExamples.section2

object Semigroups {

  // Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]

  val intCombination = naturalIntSemigroup.combine(2, 43) // addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination =
    naturalStringSemigroup.combine("Sabuj", "Jena") // concatenation

  def reduceInts(list: List[Int]): Int =
    list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String =
    list.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // Exercise 1support a new type
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance {
    (e1, e2) => Expense(Math.max(e1.id, e2.id), e1.amount + e2.amount)
  }

  // extension methods from Semigroup -|+|
  import cats.syntax.semigroup._

  val anIntSum =
    4 |+| 5 // |+| requires the presence of an implicit Semigroup[Int]
  val anExpenseSum =
    Expense(1, 23) |+| Expense(
      4,
      23
    ) // |+| requires the presence of an implicit Semigroup[Expense]

  // Excercise 2 : implement reduceThing_v2 using |+|
  def reduceThings_v2[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(_ |+| _)

  def reduceThings_v3[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _) // use context bound

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)
    println(
      reduceInts(List(1, 2, 3, 4))
    ) // compiler injects the implicit Semigroup[Int]
    println(
      reduceStrings(List("I", " love", " Scala"))
    ) // compiler injects the implicit Semigroup[String]
    println(reduceThings(List(1, 2, 3, 4)))
    println(reduceThings(List("I", " love", " Scala")))
    val numbers = (1 to 10).toList
    val numberOptions = numbers.map(Option(_))
    import cats.instances.option._ //// compiler injects the implicit Semigroup[Option[Int]]
    println(reduceThings(numberOptions))

    val expenses = List(Expense(1, 23), Expense(4, 23), Expense(3, 11))
    println(reduceThings(expenses))
    println(anExpenseSum)
    println(reduceThings_v2(expenses))
    println(reduceThings_v3(expenses))

  }

}
