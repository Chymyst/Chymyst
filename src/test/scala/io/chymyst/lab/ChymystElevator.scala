package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random.nextInt


class ChymystElevator extends FlatSpec with Matchers {

  behavior of "simulation of multi-elevator control system"

  /*
  Specification: https://medium.com/@wiemzin/elevator-control-system-using-zio-c718ae423c58

  Implementation using ZIO: https://github.com/wi101/elevator-system
  Implementations using Akka:
    https://github.com/unterstein/akka-elevator
    https://github.com/doanduyhai/elevator-control-system
  Implementation using Akka Streams: https://github.com/krausb/elevator

  - Elevator state: current floor and a list of next stops, which are all in one direction of motion from the current stops.
  - Elevators move floor by floor at fixed speed, deleting next stop from their state when reaching that floor.
  - A passenger sends a request "from floor A to floor B".
  - The system will look for a nearest elevator to floor A that is moving in the correct direction from A to B.
  - The chosen elevator will add floor A to its list of next stops.
  - The system will guide the passenger to wait for the chosen elevator, if found.
  - The passenger's request remains active until an elevator stops at floor A
    to pick up the passenger; then the request is deleted.

  Need to add three more conditions to the specification to make it complete:

  - If an elevator has no more stops to serve, it remains idle at its last visited floor.
  - If no elevators are now found that can accept a passenger's request, the request waits
    until it is accepted by some elevator. The passenger will be notified at that time.
  - The system will look for the nearest elevator that is either idle, or moving in the correct direction.

  To simulate this system using the Chemical Machine, we start by writing down
  all the data that needs to be handled concurrently.

  An elevator's state is modeled by molecule `elev`.

  Elevators will be static molecules.
  This will allow us to read their state easily and quickly, and also help with code correctness.

  The downside is that we have to define all elevators statically:
    we cannot add more elevators later, or disable some elevators. This will do for now.
   */

  type ElevNumber = Int
  type Floor = Int

  case class ElevState(current: Floor = 0, stops: List[Floor] = Nil)

  // Compute the next state of an elevator after one step.
  def step(s: ElevState, i: ElevNumber): ElevState = s match {
    case ElevState(_, Nil) ⇒ s // Idle elevator remains idle.
    case ElevState(current, next :: rest) ⇒
      val nextFloor = (next compare current) + current
      val nextStops = if (nextFloor == next) rest else next :: rest
      if (nextStops.isEmpty) println(s"Elevator $i becomes idle at floor ${s.current}")
      s.copy(current = nextFloor, stops = nextStops) // Advance to next floor.
  }

  case class Request(from: Floor, to: Floor, passenger: String)

  // An elevator may accept or refuse a request, depending on the current state.

  // Idle elevators always accept.
  def idleAccept(r: Request, state: ElevState): ElevState = {
    state.copy(stops = List(r.from, r.to))
  }

  // Return the updated elevator state if a request is accepted, otherwise `None`.
  def canAcceptRequest(r: Request, state: ElevState): Option[ElevState] = {
    state.stops.headOption match {
      case None ⇒ // Elevator is idle, accepts requests.
        Some(idleAccept(r, state))
      case Some(next) ⇒ // Elevator is at `current` floor, moving towards `next` floor.
        // The `r.from` floor needs to be between `current` and `next`, not equal to `current`.
        if ((r.from > state.current && r.from <= next) || (r.from < state.current && r.from >= next))
          if (r.from == next)
            Some(state)
          else Some(state.copy(stops = r.from :: state.stops))
        else None
    }
  }

  // The system decides whether some elevator can accept the request.
  // The elevator must be idle, or it must be moving towards floor A in the direction of floor B.
  def choose(r: Request, states: Seq[ElevState]): Option[ElevNumber] = {

    val canAccept: ((ElevState, Int)) ⇒ Boolean = {
      case (s, _) ⇒ canAcceptRequest(r, s).nonEmpty
    }

    val distance: ((ElevState, Int)) ⇒ Int = {
      case (s, _) ⇒ math.abs(s.current - r.from)
    }

    // Select elevators that are either idle or moving in the correct direction.
    // From thise, select the closest one.
    states.zipWithIndex.filter(canAccept).sortBy(distance).headOption.map(_._2)
  }

  def guidePassenger(r: Request, s: ElevState, i: ElevNumber): Unit = {
    println(s"*** Passenger ${r.passenger} at floor ${r.from} please proceed to elevator $i (currently at floor ${s.current})")
  }

  it should "run the 3-elevator system" in {

    val elev1 = m[ElevState]
    val elev2 = m[ElevState]
    val elev3 = m[ElevState]

    // Define the static reactions.
    val elev1S = go { case _ ⇒ elev1(ElevState()) }
    val elev2S = go { case _ ⇒ elev2(ElevState()) }
    val elev3S = go { case _ ⇒ elev3(ElevState()) }

    // Elevators move floor by floor with fixed speed.
    // The signal for moving will be modeled by the molecules `tick`.

    val tick1 = m[Unit]
    val tick2 = m[Unit]
    val tick3 = m[Unit]

    // Define reactions for applying one step.

    val emitAfter = m[(M[Unit], Long)]
    val emitAfterR = go { case emitAfter((t, d)) ⇒ BlockingIdle(Thread.sleep(d)); t() }

    // Elevators may have different speeds.
    val step1R = go { case elev1(s) + tick1(_) ⇒ elev1(step(s, 1)); emitAfter((tick1, 110L)) }
    val step2R = go { case elev2(s) + tick2(_) ⇒ elev2(step(s, 2)); emitAfter((tick2, 120L)) }
    val step3R = go { case elev3(s) + tick3(_) ⇒ elev3(step(s, 3)); emitAfter((tick3, 130L)) }

    // Each passenger's request, as it is being sent, is modeled by molecule `req`.
    // A delayed request is modeled by `delayed`.
    // A signal for an elevator to accept a request is `accept`.
    // A notification for passenger will be printed to stdout.

    val req = m[Request]
    val delayed = m[Request]

    val accept1 = m[Request]
    val accept2 = m[Request]
    val accept3 = m[Request]

    // A passenger sends a request. An elevator may be chosen and asked to accept the request.
    val reqR = go { case req(r) ⇒
      // Use volatile readers for elevator states.
      val states = Seq(elev1, elev2, elev3).map(_.volatileValue)
      choose(r, states) match {
        case Some(i) ⇒
          println(s"Sending an `accept` signal to elevator $i for $r")
          val accept = Seq(accept1, accept2, accept3)(i)
          accept(r)
        case None ⇒
          println(s"No elevator available, $r delayed")
          delayed(r) // Request delayed, will be accepted later by an idle elevator.
      }
    }

    // Keep track of how many requests still remain unfulfilled.
    val remain = m[Int]

    // Request will be refused if the elevator already moved past the requested floor.
    def decideAccept(r: Request, s: ElevState, i: ElevNumber, rem: Int): ElevState = {
      canAcceptRequest(r, s) match {
        case Some(newState) ⇒
          guidePassenger(r, s, i)
          remain(rem - 1)
          newState

        case None ⇒
          req(r) // Try again - perhaps another elevator can accept.
          remain(rem)
          s // Elevator state is unchanged.
      }
    }

    val acc1R = go { case accept1(r) + elev1(s) + remain(k) ⇒ elev1(decideAccept(r, s, 1, k)) }
    val acc2R = go { case accept2(r) + elev2(s) + remain(k) ⇒ elev2(decideAccept(r, s, 2, k)) }
    val acc3R = go { case accept3(r) + elev3(s) + remain(k) ⇒ elev3(decideAccept(r, s, 3, k)) }

    // Delayed requests are served by idle elevators.

    val delayed1R = go { case delayed(r) + elev1(s@ElevState(_, Nil)) + remain(k) ⇒
      elev1(idleAccept(r, s)); guidePassenger(r, s, 1); remain(k - 1)
    }
    val delayed2R = go { case delayed(r) + elev2(s@ElevState(_, Nil)) + remain(k) ⇒
      elev2(idleAccept(r, s)); guidePassenger(r, s, 2); remain(k - 1)
    }
    val delayed3R = go { case delayed(r) + elev3(s@ElevState(_, Nil)) + remain(k) ⇒
      elev3(idleAccept(r, s)); guidePassenger(r, s, 3); remain(k - 1)
    }

    // Signal when all requests are fulfilled and all elevators are idle.
    val untilFinished = b[Unit, Unit]
    val finishedR = go { case untilFinished(_, reply) + remain(0) +
      elev1(s1@ElevState(_, Nil)) + elev2(s2@ElevState(_, Nil)) + elev3(s3@ElevState(_, Nil)) ⇒
      reply()
      elev1(s1)
      elev2(s2)
      elev3(s3)
    }

    site(
      elev1S, elev2S, elev3S,
      emitAfterR,
      step1R, step2R, step3R,
      reqR,
      acc1R, acc2R, acc3R,
      delayed1R, delayed2R, delayed3R,
      finishedR
    )

    // Run the simulation: emit the initial molecules.
    Seq(tick1, tick2, tick3).foreach(_.apply())

    // Passengers send 100 requests at random times.
    val n = 100
    remain(n)

    (1 to n).foreach { i ⇒
      Thread.sleep(nextInt(100) + 100L)
      req(Request(nextInt(20) + 1, nextInt(20) + 1, s"Name-$i"))
    }

    // Wait until all passenger requests are served.
    untilFinished()
  }

  it should "run n-elevator example" in {

    val range = 5 // We have this many elevators.

    val elevs = Seq.fill(range)(m[ElevState])

    // Define the static reactions.
    val elevS = elevs map (elev ⇒ go { case _ ⇒ elev(ElevState()) })

    // Elevators move floor by floor with fixed speed.
    // The signal for moving will be modeled by the molecules `tick`.

    val ticks = Seq.fill(range)(m[Unit])

    // Define reactions for applying one step.

    val emitAfter = m[(M[Unit], Long)]
    val emitAfterR = go { case emitAfter((t, d)) ⇒ BlockingIdle(Thread.sleep(d)); t() }

    // Elevators may have different speeds.
    val stepsR = (elevs zip ticks).zipWithIndex map { case ((elev, tick), i) ⇒
      go { case elev(s) + tick(_) ⇒ elev(step(s, i)); emitAfter((tick, 100L + 10 * i)) }
    }

    // Each passenger's request, as it is being sent, is modeled by molecule `req`.
    // A delayed request is modeled by `delayed`.
    // A signal for an elevator to accept a request is `accept`.
    // A notification for passenger will be printed to stdout.

    val req = m[Request]
    val delayed = m[Request]

    val accepts = Seq.fill(range)(m[Request])

    // A passenger sends a request. An elevator may be chosen and asked to accept the request.
    val reqR = go { case req(r) ⇒
      // Use volatile readers for elevator states.
      val states = elevs.map(_.volatileValue)
      choose(r, states) match {
        case Some(i) ⇒
          println(s"Sending an `accept` signal to elevator $i for $r")
          val accept = accepts(i)
          accept(r)
        case None ⇒
          println(s"No elevator available, $r delayed")
          delayed(r) // Request delayed, will be accepted later by an idle elevator.
      }
    }

    // Keep track of how many requests still remain unfulfilled.
    val remain = m[Int]

    // Request will be refused if the elevator already moved past the requested floor.
    def decideAccept(r: Request, s: ElevState, i: ElevNumber, rem: Int): ElevState = {
      canAcceptRequest(r, s) match {
        case Some(newState) ⇒
          guidePassenger(r, s, i); remain(rem - 1); newState

        case None ⇒
          // Try again - perhaps another elevator can accept.
          // Elevator state is unchanged.
          req(r); remain(rem); s
      }
    }

    val accRs = (accepts zip elevs).zipWithIndex map { case ((accept, elev), i) ⇒
      go { case accept(r) + elev(s) + remain(k) ⇒ elev(decideAccept(r, s, i, k)) }
    }

    // Delayed requests are served by idle elevators.

    val delayedRs = elevs map { elev ⇒
      go { case delayed(r) + elev(s@ElevState(_, Nil)) + remain(k) ⇒
        elev(idleAccept(r, s)); guidePassenger(r, s, 1); remain(k - 1)
      }
    }

    // Signal when all requests are fulfilled and all elevators are idle.
    val untilFinished = b[Unit, Unit]
    val idleElevators = m[Int]
    val checkIdles = Seq.fill(range)(m[Unit])
    val checkIdleR = (checkIdles zip elevs) map { case (checkIdle, elev) ⇒
      go { case idleElevators(i) + checkIdle(_) + elev(s@ElevState(_, Nil)) ⇒ elev(s); idleElevators(i + 1) }
    }
    // Once all requests are served, we compute the number of idle elevators.
    val allDoneR = go { case remain(0) ⇒ idleElevators(0); checkIdles.foreach(_.apply()) }

    val finishedR = go { case untilFinished(_, reply) + idleElevators(`range`) ⇒ reply() }

    val allReactions = elevS ++ stepsR ++ accRs ++ delayedRs ++
      Seq(emitAfterR, reqR, finishedR, allDoneR) ++ checkIdleR

    site(allReactions: _*)

    // Run the simulation: emit the initial molecules.
    ticks.foreach(_.apply())

    // Passengers send 100 requests at random times.
    val n = 100
    remain(n)

    (1 to n).foreach { i ⇒
      Thread.sleep(nextInt(100) + 100L)
      req(Request(nextInt(20) + 1, nextInt(20) + 1, s"Name-$i"))
    }

    // Wait until all passenger requests are served.
    untilFinished()
  }
}
