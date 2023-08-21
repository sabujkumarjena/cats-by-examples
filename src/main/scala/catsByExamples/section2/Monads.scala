package catsByExamples.section2

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Monads {
  trait myMonad[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
    def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => pure(f(a)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option
  val anOptionMonad = Monad[Option]
  val anOption = anOptionMonad.pure(6)
  val aTransformedOption = anOptionMonad.flatMap(anOption)(x => Some(x + 1))

  // generic API
  def getPair[F[_], A, B](fa: F[A], fb: F[B])(implicit
      monad: Monad[F]
  ): F[(A, B)] =
    monad.flatMap(fa)(x => monad.map(fb)(y => (x, y)))

  // extension methods - pure, flatMap
  import cats.syntax.applicative._ // pure is here
  val oneOption =
    1.pure[Option] // implicit Monad[Option] will be used => Some(1)

  import cats.syntax.flatMap._ // flatMap is here
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // Monad extends Functor
  import cats.syntax.functor._ // map is here

  def getPair_v2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))
  def getPair_v3[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)
  def main(args: Array[String]): Unit = {
    println(aTransformedOption)
    println(getPair(List(1, 2, 3), List("a", "b")))
    getPair(Future(23), Future("sabuj")).foreach(println)
    println(getPair_v2(List(1, 2, 3), List("a", "b")))
    println(getPair_v3(List(1, 2, 3), List("a", "b")))
  }

}
