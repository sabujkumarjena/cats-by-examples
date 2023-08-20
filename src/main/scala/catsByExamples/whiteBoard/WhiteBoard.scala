package catsByExamples.whiteBoard

import cats.Eval
object WhiteBoard {
  val goalOfLife = Eval.later {
    println(" I am learning cats")
    45
  }
  def main(args: Array[String]): Unit = {
    println(goalOfLife)
  }

}
