package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

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
  Generic fork-join is described by the `FJTask[A]` type and two functions:
  
  def fork[A]: FJTask[A] ⇒ Either[A, List[FJTask[A]]]
  def join[A]: List[A] ⇒ A
  
  Given these two functions for a particular FJTask, we need to run this in parallel.
  
  def runFJ[FJTask[_], A](
    fork: FJTask[A] ⇒ Either[A, List[FJTask[A]]],
    join: List[A] ⇒ A,
    fjTask: FJTask[A]
  ): A
   */

  // Fibonacci numbers via fork-join.
  type FJTask[A] = A

  def fib_fork: FJTask[Int] ⇒ Either[Int, List[FJTask[Int]]] = { x ⇒
    if (x <= 2) Left(1) else Right(List(x - 1, x - 2))
  }

  def fib_join: List[Int] ⇒ Int = {
    case List(x, y) ⇒ x + y
  }

  /*
  Reference implementation: no parallelism.
   */
  it should "implement Fibonacci using fork-join sequentially" in {
    def runFJ[FJT[_], A](
      fork: FJT[A] ⇒ Either[A, List[FJT[A]]],
      join: List[A] ⇒ A,
      fjTask: FJT[A]
    ): A = fork(fjTask) match {
      case Left(result) ⇒ result
      case Right(substasks) ⇒ join(substasks.map(s ⇒ runFJ(fork, join, s)))
    }

    runFJ[FJTask, Int](fib_fork, fib_join, 8) shouldEqual 21
  }

  /*
  Reference implementation: define local reaction sites.
   */
  it should "implement Fibonacci using fork-n-join with local sites" in {

  }

  /*
  Reference implementation: define local reaction sites.
   */
  it should "implement Fibonacci using fork-n-join with continuations" in {

  }
}
