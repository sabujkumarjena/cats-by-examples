package catsByExamples.section2

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  // import cats.syntax.semigroup._ // import |+|
  // |+| is associative

  // MONOID
  import cats.Monoid
  import cats.instances.int._
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 32)
  val zero = intMonoid.empty // 0

  import cats.instances.string._ // bring implicit Monoid[String] in scope
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I lovee ", "Monoids")

  import cats.instances.option._
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption =
    Monoid[Option[Int]].combine(Option(6), Option.empty[Int]) // Some (6)
  val combineOption2 =
    Monoid[Option[Int]].combine(Option(2), Option(3)) // Some (5)
//extension methods for Monoids - |+|
  import cats.syntax.monoid._ // either this oe or cats.syntax.semigroup._

  val combineOptionFancy = Option(3) |+| Option(4)

  // Exercise 1; Implement a combineFold

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(monoid.combine)

  // extension methods for monoid

  import cats.instances.map._ //import Monoid[Map] into scope

  def combineFold_v2[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  def combineFold_v3[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  def combineFold_v4[T: Monoid](list: List[T]): T =
    list.foldLeft(implicitly[Monoid[T]].empty)(_ |+| _) // context bound

  //Exercise 3 - shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)
  import cats.instances.list._
  implicit val shoppingCardMonoid: Monoid[ShoppingCart] = Monoid.instance(ShoppingCart(List(),0),
    (sc1, sc2) => ShoppingCart(sc1.items |+| sc2.items, sc1.total |+| sc2.total )
  )
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold_v4(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(combineOptionFancy)
    println(combineFold_v4(List(1, 2, 3, 4)))
    val mapList = List(Map("alice" -> 123, "bob" -> 23), Map("jane" -> 23, "alice" -> 33))
    println(combineFold_v4(mapList))
    println(checkout(List(
      ShoppingCart(List("shoes", "iphone"), 234.4),
      ShoppingCart(List("TV"), 134.45)
    )))
  }

}
