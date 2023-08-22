package catsByExamples.section2

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  //def sumAllOptions(values: List[Option[Int]]): Int = ???
  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List] //case class OptionT[F[_], A](value: F[Option[A]])

  val listOfNumberOption: OptionT[List, Int] = OptionT(
    List(Option(1), Option(2))
  )
  val listOfCharOption: OptionT[List, Char] = OptionT(
    List(Option('a'), Option('b'), None)
  )
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOption
    number <- listOfNumberOption
  } yield (number, char)

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("hi"), Right(24), Right(23))
  )
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val futureOfEither: EitherT[Future, String, Int] =
    EitherT[Future, String, Int](Future(Right(45)))

  /*
  TODO exercise
  We have a multi machine cluster which will receive a traffic surge following a media appearance
  We measure bandwidth in units
  We want to allocate TWO of our servers to cope with the traffic spike
  We know the current capacity for each server and we know we will hold the traffic if the sum of bandwidths is > 250.
   */

  val bandwidths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server: String): AsyncResponse[Int] =
    bandwidths.get(server) match {
      case None =>
        EitherT[Future, String, Int](
          Future(Left(s"Server $server unreachable"))
        )
      case Some(b) => EitherT[Future, String, Int](Future(Right(b)))
    }

  // TODO1
  import cats.instances.future._
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    b1 <- getBandwidth(s1)
    b2 <- getBandwidth(s2)
  } yield (b1 + b2) > 250

  // TODO 2
  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = {
//    val flag = canWithstandSurge(s1,s2)
//    flag.map {
//      case true => "Yes it can withstand"
//      case false => "no"
//    }

    canWithstandSurge(s1, s2).transform {
      case Left(reason) =>
        Left(
          s"Servers $s1 and $s2 CANNOT cope with incoming spike due to : $reason"
        )
      case Right(false) =>
        Left(
          s"Servers $s1 and $s2 CANNOT cope with incoming spike due to notenough bandwidth"
        )
      case Right(true) =>
        Right(s"Servers $s1 and $s2 can cope with incoming spike ")
    }
  }

  def main(args: Array[String]): Unit = {

    println(listOfTuples)
    println(listOfTuples.value)
    generateTrafficSpikeReport("server1", "server5").value.foreach(println)

  }

}
