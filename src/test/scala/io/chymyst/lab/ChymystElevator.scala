package io.chymyst.lab

import org.scalatest.{FlatSpec, Matchers}
import io.chymyst.jc._

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

  The downside is that we have to define all elevators statically. This will do for now.
  Later, we can refactor this code to remove the repetition.
   */

  type ElevNumber = Int
  type Floor = Int

  it should "run the 3-elevator system" in {
    case class ElevState(current: Floor = 0, stops: List[Floor] = Nil)

    val elev1 = m[ElevState]
    val elev2 = m[ElevState]
    val elev3 = m[ElevState]

    // Define the static reactions. This is just boilerplate.
    val elev1S = go { case _ ⇒ elev1(ElevState()) }
    val elev2S = go { case _ ⇒ elev2(ElevState()) }
    val elev3S = go { case _ ⇒ elev3(ElevState()) }

    // Elevators move floor by floor with fixed speed.
    // The signal for moving will be modeled by the molecules `tick`.

    val tick1 = m[Unit]
    val tick2 = m[Unit]
    val tick3 = m[Unit]

    // Compute the next state of an elevator after one step.
    def step: ElevState ⇒ ElevState = {
      case s@ElevState(_, Nil) ⇒ s // Idle elevator.
      case ElevState(current, next :: rest) ⇒
        val nextFloor = (next compare current) + current
        val nextStops = if (nextFloor == next) rest else next :: rest
        ElevState(nextFloor, nextStops) // Advance to next floor.
    }

    // Define reactions for applying one step.
    val delay = m[M[Unit]]
    val delayR = go { case delay(t) ⇒ BlockingIdle(Thread.sleep(100L)); t() }
    val step1R = go { case elev1(s) + tick1(_) ⇒ elev1(step(s)); delay(tick1) }
    val step2R = go { case elev2(s) + tick2(_) ⇒ elev2(step(s)); delay(tick2) }
    val step3R = go { case elev3(s) + tick3(_) ⇒ elev3(step(s)); delay(tick3) }

    // Each passenger's request, as it is being sent, is modeled by molecule `req`.
    // A pending request is modeled by `pending`.
    // A signal for an elevator to accept a request is `accept`.
    // A notification for passenger is `proceedTo`.

    case class ReqState(from: Floor, to: Floor)

    val req = m[ReqState]
    val pending = m[ReqState]

    val accept1 = m[Floor]
    val accept2 = m[Floor]
    val accept3 = m[Floor]

    val proceedTo = m[ElevNumber]

    // The system decides whether some elevator can accept the request.
    def choose(from: Floor, to: Floor, states: Seq[ElevState]): Option[ElevNumber] = {
      ???
    }

    // A passenger sends a request.
    val reqR = go { case req(ReqState(from, to)) ⇒
      // Use volatile readers for elevator states.
      val states = Seq(elev1, elev2, elev3).map(_.volatileValue)
      choose(from, to, states) match {
        case Some(n) ⇒ // Send an `accept` signal to elevator number `n`.
          val accept = Seq(accept1, accept2, accept3)(n)
          accept(from)
          proceedTo(n)
        case None ⇒ pending(ReqState(from, to)) // Request still pending.
      }

    }

  }

}
