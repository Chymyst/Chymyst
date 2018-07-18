package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

class ChymystGoLang extends FlatSpec with Matchers {

  behavior of "goroutines as in Go lang"

  /*
  To run a computation on a parallel thread, make it into a function and emit
  that function on a molecule.
   */
  val go_routine_emit = m[() ⇒ Unit]
  site(go { case go_routine_emit(f) ⇒ f() })

  // A helper method prepares a function of type `() ⇒ Unit` and emits it.
  def go_routine[A](x: ⇒ A): Unit = go_routine_emit { () ⇒ x; () }

  it should "run a function on new thread" in {
    (1 to 10).foreach(i ⇒ go_routine {
      println(s"Running $i.")
    })
    /*
Running 1.
Running 3.
Running 2.
Running 4.
Running 7.
Running 6.
Running 8.
Running 10.
Running 5.
Running 9.
     */
  }

  behavior of "channels as in Go lang"

  /*
   Simplest API: create a channel, blocking send/receive on channel; no buffering.
   */
  final case class GoChan[A](send: B[A, Unit], receive: B[Unit, A])

  def make_chan[A]: GoChan[A] = {
    val send = b[A, Unit]
    val receive = b[Unit, A]
    site(go { case send(x, sent) + receive(_, received) ⇒ received(x); sent() })
    GoChan(send, receive)
  }

  it should "create a channel with no buffering" in {
    // Send several values from one "goroutine" to another.
    val chan = make_chan[Int]
    go_routine((1 to 10).foreach(i ⇒ chan.send(i)))
    go_routine(while (true) println("Received: " + chan.receive()))
    Thread.sleep(200)
  }

  it should "create a channel with specified buffering" in {
    /*
     Create a channel, blocking send/receive on channel; buffering to n.
     */
    final case class GoChan1[A](send: B[A, Unit], receive: B[Unit, A])

    def make_chan1[A](n: Int): GoChan1[A] = {
      val send = b[A, Unit]
      val receive = b[Unit, A]
      val ready = m[Unit]
      val buffer = m[A]
      site(
        go { case send(x, sent) + ready(_) ⇒ buffer(x) + sent() },
        go { case receive(_, received) + buffer(x) ⇒ ready() + received(x) }
      )
      (1 to n).foreach(_ ⇒ ready()) // This works for any n > 0.
      GoChan1(send, receive)
    }

    // Send several values from one "goroutine" to another.
    val chan = make_chan1[Int](2)
    chan.send(-2)
    chan.send(-1) // Should not get blocked, because of buffering of size 2!
    go_routine((1 to 10).foreach(i ⇒ chan.send(i)))
    go_routine(while (true) println("Received: " + chan.receive()))
    Thread.sleep(200)
  }

  it should "create a channel with unlimited buffering (nonblocking send)" in {
    /*
     Create a channel, blocking receive on channel but nonblocking send.
     */
    final case class GoChan2[A](send: M[A], receive: B[Unit, A])

    def make_chan2[A]: GoChan2[A] = {
      val send = m[A]
      val receive = b[Unit, A]
      site(
        go { case receive(_, received) + send(x) ⇒ received(x) }
      )
      GoChan2(send, receive)
    }

    // Send several values from one "goroutine" to another.
    val chan = make_chan2[Int]
    // Send is nonblocking now!
    (1 to 10).foreach(i ⇒ chan.send(i))
    go_routine(while (true) println("Received: " + chan.receive()))
    Thread.sleep(200)
  }

  behavior of "`select` construction as in Go lang"

  it should "select the first integer value sent, out of several" in {
    val send1 = m[Int]
    val send2 = m[Int]
    val send3 = m[Int]
    val select = m[Unit]
    site(
      go { case send1(x) + select(_) ⇒ println(s"Sent 1: $x") }
      , go { case send2(x) + select(_) ⇒ println(s"Sent 2: $x") }
      , go { case send3(x) + select(_) ⇒ println(s"Sent 3: $x") }
    )

    // Start several processes, sending integers at different times.
    go_routine {
      Thread.sleep(100); send1(100)
    }
    go_routine {
      Thread.sleep(200); send2(200)
    }
    go_routine {
      Thread.sleep(300); send3(300)
    }
    select()
    Thread.sleep(400)
  }

  it should "fail to create a `select` for Go channels that already exist" in {
    // Use a simple `GoChan` with no buffering, as implemented before.
    val chan1 = make_chan[Int]
    val chan2 = make_chan[Int]

    val send1 = chan1.send
    val send2 = chan2.send
    val select = m[Unit]

    // Cannot declare a new reaction consuming `send1` or `send2`.
    the[Exception] thrownBy site(
      go { case send1(x, r) + select(_) ⇒ r(); println(s"Sent 1: $x") }
      , go { case send2(x, r) + select(_) ⇒ r(); println(s"Sent 2: $x") }
    ) should have message "Molecule send/B cannot be used as input in Site{select + send/B → ...; select + send/B → ...} since it is already bound to Site{receive/B + send/B → ...}"

    // Reactions defined for chan1.send and chan1.receive are immutable!
  }
}
