package catsByExamples.section3

object Evaluation {

  /*
    Cats makes the distinction between
      - evaluating an expression eagerly
      - evaluating lazily and every time you request it
      - evaluating lazily and keeping the value (memoizing)
   */
  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    45
  }

  val redoEval = Eval.always {
    println("Computing again!")
    46
  }

  val delayedEval = Eval.later {
    println("Computing later!!")
    47
  }
  val composedEvaluation =
    instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // "remember a computed value"
  val dontRecompute = redoEval.memoize
  val tutorial = Eval
    .always {
      println("Step 1..."); "put the guitar on your lap"
    }
    .map { step1 =>
      println("Step 2"); s"$step1 then put your left hand on the neck"
    }
    .memoize // remember the value up to this point
    .map { steps12 =>
      println("Step 3, more complicated");
      s"$steps12 then with the right hand strike the strings"
    }

  // implement defer such that defer(Eval.now) does NOT run the side effect
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  // rewrite the methods with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(_ :+ list.head) // not stack safe

  def reverseEval_v2[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else defer(reverseEval(list.tail).map(_ :+ list.head)) // stack safe

  //
  def main(args: Array[String]): Unit = {
    // println(instantEval)
    // println(redoEval.value)
//    println(delyedEval.value)
//    println(delyedEval.value)
//    println(composedEvaluation.value)
//    println(composedEvaluation.value)
//    println(tutorial.value)

    println(reverseEval((1 to 20).toList).value)
  }

}
