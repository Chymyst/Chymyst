package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

class ChymystPubSub extends FlatSpec with Matchers {

  behavior of "broadcast publish/subscribe pattern"

  it should "demonstrate an example of broadcast" in {

    // Repeatedly emit a molecule `pub` with a given sequence of values, using given delay.

    def publish_seq[A](pub: M[A], vals: Seq[A], delay_ms: Long): M[Unit] = {
      val start = m[Unit]
      site(go { case start(_) ⇒ vals.foreach { x ⇒ pub(x); Thread.sleep(delay_ms) } })
      start
    }

    // The molecule `pub` will be emitted repeatedly, and the molecules `sub1` and `sub2`
    // will be emitted each time.
    val pub = m[Int]
    val sub1 = m[Int]
    val sub2 = m[Int]
    site(
      go { case sub1(x) ⇒ println(s"Sub1 got $x.")},
      go { case sub2(x) ⇒ println(s"Sub2 got $x.")},
      go { case pub(x) ⇒ sub1(x); sub2(x) }
    )
    publish_seq(pub, Seq(1,2,3,4,5), 100)()
    Thread.sleep(500)
  }

  /*
  We would like to create such programs from composable building blocks:
  
  - A type representing a "publisher".
  - A function to "subscribe" and "unsubscribe".
  
  A simple Pub/Sub API can look like this:
  
  - Create a "publisher" from a sequence of values.
  - Attach a "consumer" to a publisher, executing a side effect for each value.
  - Start running a "publisher"; stop it.
  - Create a new publisher by subscribing to an existing publisher, modifying its values.
  - Unsubscribe a publisher from its parent publisher. 
  
   */

  def broadcast[A](subs: M[A]*) = {
    val pub = m[A]
    site(go { case pub(x) ⇒ subs.foreach(_.apply(x)) })
    pub
  }

  it should "run publish/subscribe" in {

  }
}
