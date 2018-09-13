package io.chymyst.lab

import java.util.concurrent.atomic.AtomicInteger

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag

class ChymystForkJoin extends FlatSpec with Matchers {

  val test_n = 27 // Out of memory with local reaction sites test at test_n = 30.

  def time[A](x: ⇒ A): Long = {
    val init = System.nanoTime()
    x
    val elapsed = System.nanoTime() - init
    elapsed
  }

  behavior of "recursive fork-join"

  /*
  The non-recursive fork-join pattern (also called "map-reduce"):
  
  - A task `x` is either done sequentially (d) or forked into 2 sub-tasks (f).
  - Sub-tasks are never forked.
  - Results `r` from sub-tasks are merged (m) into the result for the task.

  Example: ternary search; find an element in a sorted array, |----.----.----|
  
     The initial task: _______    
                            x|
                          (f)|
                            / \
                          /    \
                        x|     |x
                      (d)|     |(d)
                        r|     |r
                         \    /
                          \ / 
                        (m)|
                           |
                          x|
                        (f)|
                          / \
                        /    \
                      x|     |x
                    (d)|     |(d)
                      r|     |r
                       \    /
                        \ / 
                      (m)|
                         ^
                         |_________ Return this final result.
  
  
  
  The recursive fork-join pattern:
  
  - A task `x` is either done sequentially (d) or forked into 2 sub-tasks (f).
  - Sub-tasks can be forked into sub-sub-tasks, etc., with no depth limit.
  - Results `r` from sub-tasks are merged (m) into the result for the task.
  
  Example: parallel merge-sort. Split into 2 sub-arrays, sort, merge.
    
  Computation looks like a tree:
  
     The initial task: _______    
                            x|
                          (f)|
                            / \
                          /    \
                        x|     |x
                      (f)|     |(d)
                        / \    |r
                      /    \   |
                    x|    x|   |
                  (d)|  (d)|   |
                    r|    r|   |
                     \    /    |
                      \ /      |
                    (m)|       |r
                       \      /
                        \   /      
                         \/
                      (m)|
                         ^
                         |_________ Return this final result.
                         
  For simplicity, here we forked in 2 sub-tasks. More generally, fork in `n` sub-tasks.
   */

  /*
  Generic fork-join is described by two functions:
  
  def fork[A]: A ⇒ Either[A, List[A]]
  def join[A]: List[A] ⇒ A
  
  Given these two functions for a particular task, we now need to run this in parallel.
  
  def runFJ[A](
    fork: A ⇒ Either[A, List[A]],
    join: List[A] ⇒ A,
    init_value: A
  ): A
   */

  // Fibonacci numbers via fork-join. fib(x) = f(x-1) + f(x-2).

  def fib_fork: Int ⇒ Either[Int, List[Int]] = { x ⇒
    if (x <= 2) Left(1) else Right(List(x - 1, x - 2))
  }

  def fib_join: List[Int] ⇒ Int = { // List() will always have 2 elements.
    case List(x, y) ⇒ x + y
  }

  /*
  Reference implementation: no parallelism.
   */
  it should "implement Fibonacci using fork-join sequentially" in {
    def runFJ[A](
      fork: A ⇒ Either[A, List[A]],
      join: List[A] ⇒ A,
      init_value: A
    ): A = fork(init_value) match {
      case Left(result) ⇒ result
      case Right(subtasks) ⇒ join(subtasks.map(s ⇒ runFJ(fork, join, s)))
    }

    runFJ(fib_fork, fib_join, 8) shouldEqual 21

    val elapsed = time {
      runFJ(fib_fork, fib_join, test_n)
    }
    println(f"Sequential Fibonacci($test_n) in ${elapsed / 1000000.0}%.2f ms")
  }

  /*
  Reference implementation: use Future.
   */
  it should "implement Fibonacci using fork-join with Scala Futures" in {
    def runFJ[A](
      fork: A ⇒ Either[A, List[A]],
      join: List[A] ⇒ A,
      init_value: A
    ): Future[A] = fork(init_value) match {
      case Left(result) ⇒
        Future.successful(result)
      case Right(subtasks) ⇒
        Future.sequence(subtasks.map(s ⇒ runFJ(fork, join, s))).map(join)
    }

    Await.result(runFJ(fib_fork, fib_join, 8), Duration.Inf) shouldEqual 21

    val elapsed = time {
      runFJ(fib_fork, fib_join, test_n)
    }
    println(f"Parallel/Future Fibonacci($test_n) in ${elapsed / 1000000.0}%.2f ms")
  }

  /*
  First implementation: define local reaction sites.
  
  Derivation:
  - The initial data `x: A` must be on a molecule, say `start`.
  - The final result `r: A` must be on another molecule, say `report`.
  - After we emit `start(x)`, eventually `report(r)` will be emitted.
  - When a task is split into subtasks, each subtask again emits `start()`.
  - However, the subtasks should report their results separately from the main `report`.
  Therefore, there must be a different `report` molecule for each set of subtasks.
  - We need to define a new `report` molecule every time a task is split into subtasks.
  - The results of subtasks need to be accumulated; so need a molecule carrying that data.
  Call it `accum`.
  - How does the result of `start()` know which `report` molecule to use? It cannot know.
  Therefore, we need to pass the `report` emitter as a value on `start`.
  - The `report` emitter has type `M[A]`. Hence, the type of `start` must be `M[(A, M[A])].
   */
  it should "implement Fibonacci using fork-join with local sites" in {
    def runFJ[A](
      fork: A ⇒ Either[A, List[A]],
      join: List[A] ⇒ A
    ): M[(A, M[A])] = { // Returns a new molecule emitter, called `start`.

      val start = m[(A, M[A])] // This molecule carries (init_x, report_result).

      site(go { case start((x, report)) ⇒
        fork(x) match {
          case Left(result) ⇒ report(result)
          case Right(subtasks) ⇒ // Define a reaction for the sub-tasks.
            val sub_report = m[A] // The `report` molecule of a sub-task.
          val accum = m[List[A]] // List of results computed by sub-tasks so far.
            site(go { case sub_report(res) + accum(xs) ⇒
              val new_xs = res :: xs
              // Are all subtasks done?
              if (new_xs.length == subtasks.length) report(join(new_xs))
              else accum(new_xs)
            })
            accum(Nil)
            subtasks.foreach(s ⇒ start((s, sub_report)))
        }
      })
      start
    }

    val report_result = m[Int]
    val get_result = b[Unit, Int]
    site(go { case report_result(x) + get_result(_, r) ⇒ r(x) })
    runFJ(fib_fork, fib_join)((8, report_result))
    get_result() shouldEqual 21

    val elapsed = time {
      runFJ(fib_fork, fib_join)((test_n, report_result));
      get_result()
    }
    println(f"Parallel/Local sites Fibonacci($test_n) in ${elapsed / 1000000.0}%.2f ms")
  }

  /*
  Second implementation: use a single reaction site but local continuations.
  
  Each new reaction site introduces extra overhead. We can instead use continuations:
  Each `start()` molecule will carry a continuation to be called on its result.
  When a task is split, we put new continuations on the subtasks' `start()` molecules.
  These continuations will collect and merge partial results, and report them as needed.
  
  It will be easier to implement the collect/merge using an `AtomicInteger` for counting. 
   */
  it should "implement Fibonacci using fork-join with continuations" in {
    def runFJ[A: ClassTag](
      fork: A ⇒ Either[A, List[A]],
      join: List[A] ⇒ A
    ): M[(A, A ⇒ Unit)] = { // Returns a new molecule emitter, called `start`.

      val start = m[(A, A ⇒ Unit)] // The molecule carries (init_x, consume_result).
      // `consume_result` is a continuation that must be called when this task is done.

      site(go { case start((x, consume)) ⇒
        fork(x) match {
          case Left(result) ⇒ consume(result)
          case Right(subtasks) ⇒
            // The continuation for the sub-tasks will close over `counter` and `accum`.
            val counter = new AtomicInteger(subtasks.length) // The counter will go to 0.
          val accum = new Array[A](subtasks.length) // Sub-tasks will put results here.
          val sub_report: A ⇒ Unit = { x ⇒
            val newCounter = counter.decrementAndGet()
            accum.update(newCounter, x)
            if (newCounter == 0) // All subtasks are finished.
              consume(join(accum.toList))
          }
            subtasks.foreach(s ⇒ start((s, sub_report)))
        }
      })
      start
    }

    val report_result = m[Int]
    val get_result = b[Unit, Int]
    site(go { case report_result(x) + get_result(_, r) ⇒ r(x) })
    val start = runFJ(fib_fork, fib_join)

    start((8, report_result))
    get_result() shouldEqual 21

    val elapsed = time {
      start((test_n, report_result))
      get_result()
    }
    println(f"Parallel/Continuations Fibonacci($test_n) in ${elapsed / 1000000.0}%.2f ms")
  }

  // See https://scalaz.github.io/scalaz-zio/usage/fibers.html
  it should "implement Fibonacci using scalaz-zio" in {
    import scalaz.zio._

    def fib(n: Int): IO[Nothing, Int] = {
      if (n <= 1) {
        IO.point(1)
      } else {
        for {
          fiber1 <- fib(n - 2).fork
          fiber2 <- fib(n - 1).fork
          v2 <- fiber2.join
          v1 <- fiber1.join
        } yield v1 + v2
      }
    }

    val elapsed = time {
      new RTS{}.unsafeRun(fib(test_n))
    }
    println(f"scalaz-zio Fibonacci($test_n) in ${elapsed / 1000000.0}%.2f ms")
  }
}
