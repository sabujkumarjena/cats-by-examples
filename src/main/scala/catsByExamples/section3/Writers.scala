package catsByExamples.section3

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {
  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] =
    Writer(List("Started Something"), 45) // value increases, logs stay the same

  // 2- manipulate  them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1)
  val aLogsWriter = aWriter.mapWritten(
    _ :+ "found something interesting"
  ) // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(
    _ :+ "found something interesting",
    _ + 1
  ) // both loga and value change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }
  import cats.instances.vector._ // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 30)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va * vb

//reset the logs
  import cats.instances.list._ // an implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value
//3 - dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run
//TODO 1: rewrite a function which "prints" things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }
  def countAndSay_v2(n: Int): Unit = {
    def recFun(
        count: Int,
        result: Writer[List[String], Int]
    ): Writer[List[String], Int] = {
      if (count == 0) result.mapWritten(list => "starting!\n" :: list)
      else
        recFun(
          count - 1,
          result.mapWritten(list => count.toString + "\n" :: list)
        )
    }
    recFun(n, Writer(List.empty[String], n)).written.foreach(print)
  }
  def countAndLog_v3(n: Int): Writer[List[String], Int] = {
    if (n <= 0) Writer(List("starting!"), 0)
    else countAndLog_v3(n - 1).flatMap(_b => Writer(List(s"$n"), n))
  }

  // Benefit #1 : we work with pure FP
  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum (${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSum_v2(n: Int): Writer[List[String], Int] =
    if (n <= 0) Writer(List.empty[String], 0)
    else for {
      _ <- Writer(List(s"Now at $n"), n)
      lowerSum <- naiveSum_v2(n-1)
      _ <- Writer(List(s"Computed sum(${n -1}) = $lowerSum"), n)
    } yield lowerSum + n

  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(anEmptyWriter.run)
    countAndSay_v2(10)
    countAndLog_v3(10).written.foreach(println)
    naiveSum(10)
    naiveSum_v2(10).written.foreach(println)
    println("***************")
    implicit val ec: ExecutionContext =  ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(naiveSum_v2(100))
    val sumFuture2 = Future(naiveSum_v2(100))
    val logs1 = sumFuture1.map(_.written) //logs from thread 1
    val logs2 = sumFuture2.map(_.written) //logs from thread 2
  }

}
