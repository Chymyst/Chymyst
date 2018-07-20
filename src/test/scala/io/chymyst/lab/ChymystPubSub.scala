package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

class ChymystPubSub extends FlatSpec with Matchers {

  behavior of "broadcast publish/subscribe pattern"

  it should "demonstrate an example of broadcast" in {

    // Repeatedly emit a molecule `pub` with a given sequence of values, using given delay.

    def publish_seq[A](pub: M[A], xs: Seq[A], delay_ms: Long): M[Unit] = {
      val start = m[Unit]
      site(go { case start(_) ⇒ xs.foreach { x ⇒ pub(x); Thread.sleep(delay_ms) } })
      start
    }

    // The molecule `pub` will be emitted repeatedly, and the molecules
    // `sub1` and `sub2` will be emitted each time after that.
    val pub = m[Int]
    val sub1 = m[Int]
    val sub2 = m[Int]
    site(
      go { case sub1(x) ⇒ println(s"Sub1 got $x.") },
      go { case sub2(x) ⇒ println(s"Sub2 got $x.") },
      go { case pub(x) ⇒ sub1(x); sub2(x) }
    )
    publish_seq(pub, Seq(1, 2, 3, 4, 5), 100)()
    Thread.sleep(500)
  }

  /*
  We would like to create such programs from composable building blocks:
  
  - A type representing a "publisher".
  - Functions to "subscribe" and "unsubscribe".
  
  A simple Pub/Sub API can look like this:
  
  - Create a "publisher" from a sequence of values.
  - Start running a "publisher"; stop it.
  - Attach a "consumer" to a publisher, executing a side effect for each value.
  - Create a new publisher by subscribing to an existing publisher.
  - Unsubscribe a publisher from its parent publisher. 

  A typical pattern for encapsulating concurrent logic in the chemical machine:
  - A function takes some molecules as arguments and returns new molecules as results
  - The arguments are molecules that are bound elsewhere
  - The results are molecules that are bound within the function's scope

   */

  // Publisher exposes molecules that can be emitted to perform the operations.
  case class Publisher[A](send_data: M[List[A]], stop: B[Unit, Unit], pub: M[A])

  def make_pub[A](pub: M[A], delay_ms: Long): Publisher[A] = {
    val data = m[List[A]]
    val stop = b[Unit, Unit]
    site(
      go { case data(x :: tail) ⇒ pub(x); Thread.sleep(delay_ms); data(tail) },
      go { case stop(_, r) + data(_) ⇒ r() } // Once stopped, can't resume.
    )
    Publisher(data, stop, pub)
  }

  // Attach a consumer ("sink"). Once attached, can't be detached.
  def attach[A](pub: M[A])(f: A ⇒ Unit): Unit = site(go { case pub(x) ⇒ f(x) })

  // Subscribe/unsubscribe mechanism.
  case class Office[A](subscribe: M[M[A]], unsubscribe: M[M[A]])

  def make_office[A](pub: M[A]): Office[A] = {
    val subs = m[List[M[A]]]
    val subscribe = m[M[A]]
    val unsubscribe = m[M[A]]
    site(
      go { case pub(x) + subs(ms) ⇒ ms.foreach(s ⇒ s(x)); subs(ms) },
      go { case subs(ms) + subscribe(s) ⇒ subs(s :: ms) },
      go { case subs(ms) + unsubscribe(s) ⇒ subs(ms.diff(List(s))) },
      go { case _ ⇒ subs(Nil) }
    )
    Office(subscribe, unsubscribe)
  }

  it should "run publisher, consumer, subscribe, and unsubscribe" in {
    val pub = m[Int]
    val office = make_office(pub)
    val publisher = make_pub(pub, 100)
    // Some subscribers.
    val s1 = m[Int]
    val s2 = m[Int]
    // Attach some consumer functions to these subscribers.
    attach(s1)(x ⇒ println(s"s1 got $x."))
    attach(s2)(x ⇒ println(s"s2 got $x."))
    // Start publishing.
    publisher.send_data((1 to 10).toList)
    // Subscribe the first consumer.
    office.subscribe(s1)
    // Wait a bit, then subscribe the second consumer.
    Thread.sleep(300)
    office.subscribe(s2)
    // Wait a bit, then unsubscribe the first consumer.
    Thread.sleep(300)
    office.unsubscribe(s1)
    Thread.sleep(300)
    publisher.stop()
    println("Stopped the publisher.")
    Thread.sleep(500) // See if things actually stop.
    /* Printed output:
s1 got 1.
s1 got 2.
s1 got 3.
s2 got 4.
s1 got 4.
s2 got 5.
s1 got 5.
s2 got 6.
s1 got 6.
s2 got 7.
s2 got 8.
s2 got 9.
s2 got 10.
Stopped the publisher.
     */
  }
}
