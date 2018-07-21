package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class ChymystForkJoin extends FlatSpec with Matchers {

  behavior of "recursive fork-join"

  /*
  The non-recursive fork-join pattern (also called "map-reduce"):
  
  - A task is either done sequentially (d) or forked into 2 sub-tasks (f).
  - Sub-tasks are never forked.
  - Results from sub-tasks are combined (c) into the result for the task.

  (Example: ternary search.)
  
     The initial task: _______    
                             |
                          (f)|
                            / \
                          /    \
                         |     |
                      (d)|     |(d)
                         \    /
                          \ / 
                        (c)|
                           |
                        (f)|
                          / \
                        /    \
                       |     |
                    (d)|     |(d)
                       \    /
                        \ / 
                      (c)|
                         ^
                         |_________ Return this final result.
  
  
  
  The recursive fork-join pattern:
  
  - A task is either done sequentially (d) or forked into 2 sub-tasks (f).
  - Sub-tasks can be forked into sub-sub-tasks, etc., with no depth limit.
  - Results from sub-tasks are combined (c) into the result for the task.
  
  (Example: parallel merge-sort.)
    
  Computation looks like a tree:
  
     The initial task: _______    
                             |
                          (f)|
                            / \
                          /    \
                         |     |
                      (f)|     |(d)
                        / \    |
                      /    \   |
                     |     |   |
                  (d)|  (f)|   |
                     \    /    |
                      \ /      |
                    (c)|       |
                       \      /
                        \   /      
                         \/
                      (c)|
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

  // Fibonacci numbers via fork-join.

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
  }

  /*
  First implementation: define local reaction sites.
   */
  it should "implement Fibonacci using fork-join with local sites" in {
    def runFJ[A](
      fork: A ⇒ Either[A, List[A]],
      join: List[A] ⇒ A
    ): (A, M[A]) ⇒ Unit = {
      val run_fj = m[(A, M[A])] // This molecule carries (init_value, report_result).

      site(go { case run_fj((x, report)) ⇒
        fork(x) match {
          case Left(result) ⇒ report(result)
          case Right(subtasks) ⇒
            // Define a reaction for the sub-tasks.
            val sub_done = m[A]
            val accum = m[List[A]]
            site(go { case sub_done(res) + accum(xs) ⇒
              val new_xs = res :: xs
              // Are all subtasks done?
              if (new_xs.length == subtasks.length) report(join(new_xs))
              else accum(new_xs)
            })
            accum(Nil)
            subtasks.foreach(s ⇒ run_fj((s, sub_done)))
        }
      })
      (init_value, report_result) ⇒ run_fj((init_value, report_result))
    }

    val report_result = m[Int]
    val get_result = b[Unit, Int]
    site(go { case report_result(x) + get_result(_, r) ⇒ r(x) })
    runFJ(fib_fork, fib_join)(8, report_result)
    get_result() shouldEqual 21
  }

  /*
  Second implementation: use a single reaction site but local continuations.
   */
  it should "implement Fibonacci using fork-join with continuations" in {

  }
}
