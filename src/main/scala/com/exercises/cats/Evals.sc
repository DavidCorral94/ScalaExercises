object Evals {

  /**
    * Summary
    * Eval.now => its evaluated in the moment
    * Eval.later => its evaluated when is required
    * Eval.always => its (r)evaluated in each call
    * Eval.defer => delays a computation
    */

  import cats._

  // ==> EVAL NOW

  val eagerEval = Eval.now {
    println("This is eagerly evaluated")
    1 :: 2 :: 3 :: Nil
  }

  eagerEval.value == List(1, 2, 3)

  val evalNow = Eval.now {
    println("Evaluating now")
    5
  }

  evalNow.value
  evalNow.value
  evalNow.value

  // ==> EVAL LATER

  val lazyEval = Eval.later {
    println("This is lazy evaluated")
    1 :: 2 :: 3 :: Nil
  }

  println("Doing some stuff")

  lazyEval.value == List(1, 2, 3)

  val n = 2
  var counter = 0
  val lazyEvalv2 = Eval.later {
    println("This is lazyly evaluated with caching")
    counter = counter + 1
    (1 to n)
  }

  List.fill(n)("").foreach(_ => lazyEval.value)
  lazyEvalv2.value == Range(1, 3)
  counter = 1

  // ==> EVAL ALWAYS

  val alwaysEval = Eval.always(println("Always evaluated"))

  alwaysEval.value
  alwaysEval.value
  alwaysEval.value

  val n2 = 4
  var counter2 = 0
  val alwaysEvalv2 = Eval.always {
    println("This is lazyly evaluated without caching")
    counter2 = counter2 + 1
    (1 to n2)
  }

  //when/then
  List.fill(n2)("").foreach(_ => alwaysEvalv2.value)
  counter2 == 4
  alwaysEvalv2.value == Range(1, 5)
  counter2 == 5

  // ==> EVAL DEFER

  val list = List.fill(3)(0)

  //when
  val deferedEval: Eval[List[Int]] =
    Eval.now(list).flatMap(e => Eval.defer(Eval.later(e)))

  //then
  Eval.defer(deferedEval).value == List(0, 0, 0)
}
