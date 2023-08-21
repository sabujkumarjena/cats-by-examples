package catsByExamples.section2

import scala.util.Try

object Functors {

  // simplified functor
  trait MyFunctor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // include Functor[List]
  val listFunctor = Functor[List]
  val increamentedNumbers = listFunctor.map(List(1, 2, 3, 4))(_ + 1)

  // generalizing an API

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]) : Tree[T] = Branch(value, left, right)
  }

  // Exercise 1: define your own functor for a binary tree
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(a)         => Leaf(f(a))
      case Branch(n, l, r) => Branch(f(n), map(l)(f), map(r)(f))
    }
  }

  // etension methods - map

  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(30, Tree.leaf(10), Tree.leaf(40))
  val tree10x = tree.map(_*10)

  //Exercise  2 : white a shorted do10x method using extension methos
  def do10x_v2[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_*10)

  def main(args: Array[String]): Unit = {
    println(increamentedNumbers)
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(5)))
    println(do10x(Try(5)))
    println(do10x[Tree](Branch(30, Leaf(10), Leaf(40))))
    println(do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(40))))
    println(tree10x)
    do10x_v2(tree)
  }

}
