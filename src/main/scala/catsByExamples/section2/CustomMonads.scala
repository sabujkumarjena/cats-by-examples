package catsByExamples.section2

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None           => None
        case Some(Left(v))  => tailRecM(v)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdenitityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(
        f: A => Identity[B]
    ): Identity[B] = f(fa)

    override def tailRecM[A, B](
        a: A
    )(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(b) => b
    }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Leaf(x)      => f(x)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v))  => stackRec(f(v))
        case Leaf(Right(v)) => Leaf(v)
        case Branch(l, r)   => Branch(stackRec(l), stackRec(r))
      }

      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          done: List[Tree[B]]
      ): Tree[B] =
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v))  => tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(v)) => tailRec(todo.tail, expanded, Leaf(v) :: done)
            case node @ Branch(left, right) =>
              if (!expanded(node)) {
                tailRec(right :: left :: todo, expanded + node, done)

              } else {
                val newLeft = done.head
                val newRight = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }

          }
      tailRec(List(f(a)), Set(), List())
      // stackRec(f(a))
    }
  }
  /*
              _____1_____
           __2__       __3__
          /     \     /     \
         L1     R2   R3     R4

          tr([1], [], []) =
          tr([3, 2, 1], [1], []) =
          tr([R4, R3, 3, 2, 1], [3, 1], []) =
          tr([R3, 3, 2, 1], [3, 1], [B4]) =
          tr([3, 2, 1], [3, 1], [B3, B4]) =
          tr([2, 1], [1], [B34]) =
          tr([R2, L1, 2, 1], [2, 1], [B34]) =
          tr([L1, 2, 1], [2, 1], [B2, B34]) =
          tr([R1, 2, 1], [2, 1], [B2, B34]) =
          tr([2,1], [2, 1], [B1, B2, B34]) =
          tr([1], [1], [B12, B34]) =
          tr([], [], [B1234]) =
          B1234

   */

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(30))
    val changedTree =
      TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 1)))
    println(changedTree)
  }

}
