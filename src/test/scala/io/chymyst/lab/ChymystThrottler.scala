package io.chymyst.lab

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.jc._

class ChymystThrottler extends FlatSpec with Matchers {

  behavior of "Throttler"
  /*
  A molecule `s(x)` already exists and has some reactions defined for it.
  We need to throttle any external code that would emit `s()`.

  How can we implement this?

  Given `s`, we will produce a new molecule `r` that represents a "request"
  to emit s. The value carried by `r` is the same as that carried by `s`.
  The molecule `r` can be emitted at will, but it somehow should cause `s`
  to be emitted at a limited rate. More precisely, no `s` may be emitted earlier
  than `delta_t` milliseconds after any previous `s` was emitted.
  */

  /* We can implement a function producing a new molecule emitter.

  def throttle[A](s: M[A], delta_t: Long): M[A] = {
    val r = m[A]
    site(go { case r(x) ⇒ ??? }) // Define a new reaction consuming `r`.
    r
  }

  The new reaction must consume `r(x)` and should emit `s(x)` only if `s` has not
  yet been emitted too frequently. Suppose that the reaction body tries to decide
  whether to emit an `s(x)` by evaluating some condition:

  go { case r(x) ⇒ if (???) s(x) }

  The condition should evaluate to `false` when `r(x)` has been emitted too soon.
  In that case, we would need to delay the emission of `s` until a later time.
  But this reaction could be running concurrently with multiple copies of `r(x)`.
  There is not enough information to decide how long to delay the emission of `s`.

  We need to prevent this reaction from starting, until at least `delta_t` ms
  after a previous `s` was emitted.

  A reaction can be prevented from starting if we withhold an input molecule.
  Therefore, this reaction must have another input molecule, say `allow()`:

  go { case r(x) + allow(_) ⇒ s(x); ??? }

  We need to control the presence of `allow()`. If `allow()` is present, an `s(x)`
  will be emitted. By requirement, the next request `r(x)` can be processed after
  at least `delta_t` ms has elapsed. If we emit `allow()` in this reaction right
  away, a next `r(x)` could start another reaction immediately. Therefore, we need
  to delay emitting `allow()` in this reaction.

  go { case r(x) + allow(_) ⇒ s(x); Thread.sleep(delta_t); allow() }

  The complete code of `throttle()` now looks like this:


  def throttle[A](s: M[A], delta_t: Long): M[A] = {
    val r = m[A]
    val allow = m[Unit]
    site(go { case r(x) + allow(_) ⇒ s(x); Thread.sleep(delta_t); allow() })
    allow() // Beginning of time; we allow requests.
    r
  }

  Is it satisfactory that we do a `Thread.sleep()` inside a reaction?
  This blocks a thread, and we might want to avoid that.
  However, in this case:

  - there will be only one copy of `allow()`, so we will only block one thread
  - the logic will work correctly even if the reaction site runs on a single thread
  - an alternative implementation would be to use a `Timer`, which will statically
    allocate a new thread and have it a blocked most of the time

  It seems that the straightforward implementation via `Thread.sleep()` is adequate.

  We can still improve the code in two ways:

  - make `allow()` a static molecule, so that we will not accidentally forget it
  - use `BlockingIdle()` around `Thread.sleep()`, to improve parallelism

  The final code becomes:
  */
  def throttle[A](s: M[A], delta_t: Long): M[A] = {
    val r = m[A]
    val allow = m[Unit]
    site(
      go { case r(x) + allow(_) ⇒
        s(x); BlockingIdle(Thread.sleep(delta_t)); allow()
      },
      go { case _ ⇒ allow() } // Beginning of time; we allow requests.
    )
    r
  }

  // Define a reaction that prints the time elapsed since the previous run.
  // We will then throttle this reaction and see what it prints.
  def print_elapsed(message: String): M[Unit] = {
    val timestamp = m[Long]
    val s = m[Unit]
    site(go { case s(_) + timestamp(t) ⇒
      val new_timestamp = System.currentTimeMillis()
      println(s"$message Time interval: ${new_timestamp - t}")
      timestamp(new_timestamp)
    })
    timestamp(0)
    s
  }

  it should "run a reaction without throttling" in {
    val s = print_elapsed("Unthrottled.")
    // First, produce some quick s() emissions.
    (1 to 10).foreach(_ ⇒ s())
    Thread.sleep(100) // We need to wait a little before stopping the test.
    /*  This prints some output such as:

    Unthrottled. Time interval: 1531692509234
    Unthrottled. Time interval: 1
    Unthrottled. Time interval: 1
    Unthrottled. Time interval: 0
    Unthrottled. Time interval: 0
    Unthrottled. Time interval: 1

    and so on. */
  }
  it should "run a reaction with 200 ms throttling" in {
    val s = print_elapsed("Throttled.")
    val interval = 200L
    val r = throttle(s, interval)

    (1 to 10).foreach(_ ⇒ r())

    Thread.sleep(10 * interval) // We need to wait more before stopping the test.

    /*  This prints some output such as:

Throttled. Time interval: 1531693788450
Throttled. Time interval: 205
Throttled. Time interval: 202
Throttled. Time interval: 205
Throttled. Time interval: 202
Throttled. Time interval: 204

    and so on. */
  }

  /*
  We have implemented a generic throttle function in about 10 lines of code.
  The initial implementation was 6 lines of code. We have derived the code by
  straightforward logical reasoning.

  As a comparison, here is an implementation of throttling in Akka, Monix, and ZIO.
  https://github.com/softwaremill/akka-vs-scalaz/tree/master/core/src/main/scala/com/softwaremill/ratelimiter

  Akka, Monix, and ZIO require about 40 lines of code each (excluding imports and
  comments), and additionally about 30 lines of common code (`RateLimiterQueue`).
  Akka-Typed is about 35 lines of code.

  The code in the chemical machine is very concise and declarative in comparison.
   */
}
