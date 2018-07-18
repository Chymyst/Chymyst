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
  case class GoChan[A](send: B[A, Unit], receive: B[Unit, A])

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
        go { case send(x, sent) + ready(_) ⇒ buffer(x); sent() },
        go { case receive(_, received) + buffer(x) ⇒ ready(); received(x) }
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
    val select = b[Unit, Unit]
    site(
      go { case send1(x) + select(_, r) ⇒ r(); println(s"Sent 1: $x.") },
      go { case send2(x) + select(_, r) ⇒ r(); println(s"Sent 2: $x.") },
      go { case send3(x) + select(_, r) ⇒ r(); println(s"Sent 3: $x.") }
    )

    // Start several processes, sending integers at different times.
    go_routine {
      Thread.sleep(100)
      send1(100)
    }
    go_routine {
      Thread.sleep(200)
      send2(200)
    }
    go_routine {
      Thread.sleep(300)
      send3(300)
    }
    select()
    select()
    Thread.sleep(400) // Wait for things to happen.
    /* Prints output:
Sent 1: 100.
Sent 2: 200.
     */
  }

  it should "select as in Go lang, with default" in {
    val send1 = m[Int]
    val send2 = m[Int]
    val send3 = m[Int]
    val select = b[Unit, Unit]
    val default_select = m[Unit]
    site(
      go { case default_select(_) + select(_, r) ⇒ r(); println("Default.") },
      go { case send1(x) + select(_, r) ⇒ r(); println(s"Sent 1: $x.") },
      go { case send2(x) + select(_, r) ⇒ r(); println(s"Sent 2: $x.") },
      go { case send3(x) + select(_, r) ⇒ r(); println(s"Sent 3: $x.") }
    )

    // Start several processes, sending integers at different times.
    go_routine {
      Thread.sleep(100)
      send1(100)
    }
    go_routine {
      Thread.sleep(200)
      send2(200)
    }
    go_routine {
      Thread.sleep(300)
      send3(300)
    }
    // Activate default after timeout of 150 ms.
    go_routine {
      Thread.sleep(150)
      default_select()
    }
    select()
    select()
    Thread.sleep(400) // Wait for things to happen.
    /* Prints output:
Sent 1: 100.
Default.
     */
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
      go { case send1(x, r) + select(_) ⇒ r(); println(s"Sent 1: $x") },
      go { case send2(x, r) + select(_) ⇒ r(); println(s"Sent 2: $x") }
    ) should have message "Molecule send/B cannot be used as input in Site{select + send/B → ...; select + send/B → ...} since it is already bound to Site{receive/B + send/B → ...}"

    // Reactions defined for chan1.send and chan1.receive are immutable!
  }

  // An example given in "Advanced Go concurrency patterns" (by Sameer Ajmani)
  // https://talks.golang.org/2013/advconc.slide#6
  /* The program in Go lang:
type Ball struct{ hits int }

func main() {
    table := make(chan *Ball)
    go player("ping", table)
    go player("pong", table)

    table <- new(Ball) // game on; toss the ball
    time.Sleep(1 * time.Second)
    <-table // game over; grab the ball
}

func player(name string, table chan *Ball) {
    for {
        ball := <-table
        ball.hits++
        fmt.Println(name, ball.hits)
        time.Sleep(100 * time.Millisecond)
        table <- ball
    }
}
   */
  it should "play ping-pong as in Go lang" in {
    type Ball = Int
    val table = make_chan[Ball]
    go_routine(player("ping", table))
    go_routine(player("pong", table))
    table.send(0) // Game on; toss the ball.
    Thread.sleep(1000)
    table.receive() // Game over; grab the ball.

    def player(name: String, table: GoChan[Ball]): Unit = {
      while (true) {
        val ball = 1 + table.receive()
        println(s"$name, $ball")
        Thread.sleep(100)
        table.send(ball)
      }
    }

    /* This works, but sometimes the output is:
ping, 1
pong, 2
ping, 3
pong, 4
...
      while at other times, the output is:
pong, 1
ping, 2
pong, 3
ping, 4
...
     */
  }

  it should "play ping-pong via idiomatic Chemical Machine code" in {
    /*
    Required functionality:
    - A chosen player (either `ping` or `pong`) starts the game.
    - Moves are counted and messages are printed as before.
    - A blocking signal `stop` will stop the game.
     */
    val ball = m[Int] // The current counter value.
    val stop = b[Unit, Int] // Return the number of moves played.
    val side = m[String] // The current player's name.

    site(
      go { case ball(n) + side(name) ⇒ play(name, n + 1) },
      go { case ball(n) + stop(_, reply) ⇒ reply(n) }
    )

    def play(name: String, n: Int): Unit = {
      println(s"$name, $n")
      Thread.sleep(100)
      val other_name = if (name == "ping") "pong" else "ping"
      ball(n)
      side(other_name)
    }

    side("ping") // First player is `ping`.
    ball(0) // Start game.
    Thread.sleep(1000)
    val r = stop() // This may take some time!
    println(s"Game over after $r moves.")
    Thread.sleep(1000) // If the game is really over, nothing will be printed here.
  }

  it should "play ping-pong with more reliable stopping" in {
    val ball = m[Int] // The current counter value.
    val stop = b[Unit, Int] // Return the number of moves played.
    val stopped = m[Int] // Game status.
    val side = m[String] // The current player's name.

    site(
      go { case ball(n) + side(name) ⇒ play(name, n + 1) },
      go { case stopped(n) + stop(_, reply) ⇒ reply(n) }
    )

    def play(name: String, n: Int): Unit = {
      if (name == "stop") {
        stopped(n)
      } else {
        println(s"$name, $n")
        Thread.sleep(100)
        val other_name = if (name == "ping") "pong" else "ping"
        ball(n)
        side(other_name)
      }
    }

    def stop_game(): Int = {
      side("stop")
      stop()
    }

    side("ping") // First player is `ping`.
    ball(0) // Start game.
    Thread.sleep(1000)
    val r = stop_game() // This may take some time!
    println(s"Game over after $r moves.")
    Thread.sleep(1000) // If the game is really over, nothing will be printed here.
  }
}
