package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

class ChymystConsensus extends FlatSpec with Matchers {

  behavior of "simple multi-thread consensus object"

  /*
  A simple "consensus object" provides a function `decide()` that can be called
  concurrently with different values but returns the same value in all threads.

  (This is _not_ a distributed consensus; all computations run on the same JVM.)

  How to implement it using the chemical machine?

  We must use a blocking molecule `decide()`. Assume that the values have type X.

  The blocking molecule needs to be consumed by some reaction:

  go { case decide(x, r) ⇒ ??? }

  If we do this, multiple reactions may run if `decide()` is emitted many times.
  To prevent that, we need to introduce another input molecule:

  go { case decide(x, r) + consensus(???) ⇒ ??? }

  We need to represent the information about whether we already have a consensus.
  It is natural to put that information on the `consensus()` molecule as Option[X].
  The reaction then needs to reply to `decide()` with the consensus value, if any.

  go { case decide(x, r) + consensus(c) ⇒ ??? }

  If c is None, we have no consensus yet; so we can set it to `x` and reply with `x`.
  If c is Some(y), we already have consensus, and it is `y`. So we reply with `y`.
   */
  def make_co[X]: B[X, X] = {
    val decide = b[X, X]
    val consensus = m[Option[X]]
    site(
      go { case decide(x, r) + consensus(c) ⇒
        val decision = c.getOrElse(x)
        r(decision); consensus(Some(decision))
      },
      go { case _ ⇒ consensus(None) } // So that `consensus()` is a static molecule.
    )
    decide
  }

  /*
To run a computation on a parallel thread, make it into a function and emit
that function on a molecule.
 */
  val go_routine_emit = m[() ⇒ Unit]

  site(go { case go_routine_emit(f) ⇒ f() })

  // A helper method prepares a function of type `() ⇒ Unit` and emits it.
  def go_routine[A](x: ⇒ A): Unit = go_routine_emit { () ⇒ x; () }


  it should "run multiple processes and achieve consensus" in {
    val decide = make_co[Int]
    decide.setLogLevel(4)
    (1 to 10).foreach { i ⇒ go_routine(println(decide(i))) }
    Thread.sleep(1000)

    /* Printed result:
5
5
5
5
5
5
5
5
5
5
     */
  }
}
