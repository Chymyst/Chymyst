package io.chymyst.lab

import io.chymyst.jc._

import org.scalactic.source.Position
import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.{Millis, Span}
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.Waiters.{PatienceConfig, Waiter}
import scala.concurrent.duration.DurationInt
import scala.concurrent.Future
import scala.language.implicitConversions

class LabSpec extends FlatSpec with Matchers with TimeLimitedTests {

  val timeLimit = Span(2000, Millis)

  val warmupTimeMs = 50L

  val patienceConfig = PatienceConfig(timeout = Span(1000, Millis))

  def waitSome(): Unit = Thread.sleep(warmupTimeMs)

  behavior of "future + molecule"

  it should "emit a molecule from a future computed out of a given future" in {

    val c = m[Unit]
    val f = b[Unit, Unit]

    val tp = FixedPool(2)
    implicit val ec = tp.executionContext
    
    site(tp)(
      go { case c(_) + f(_, r) => r() }
    )

    Future {
      Thread.sleep(50)
    } & c // insert a molecule from the end of the future

    f() shouldEqual (())
  }

  it should "emit a molecule from a future with a lazy emission" in {
    val waiter = new Waiter

    val c = new M[String]("c")
    val tp = FixedPool(2)
    implicit val _ = tp.executionContext

    site(tp)(
      go { case c(x) => waiter {
        x shouldEqual "send it off"; ()
      }; waiter.dismiss()
      }
    )

    Future {
      Thread.sleep(50)
    } & c("send it off") // insert a molecule from the end of the future

    waiter.await()(patienceConfig, implicitly[Position])

  }

  it should "not emit a molecule from a future prematurely" in {
    val waiter = new Waiter

    val c = m[Unit]
    val d = m[Unit]
    val e = m[Unit]
    val f = b[Unit, String]
    val f2 = b[Unit, String]

    val tp = FixedPool(4)
    implicit val _ = tp.executionContext

    site(tp)(
      go { case e(_) + c(_) => d() },
      go { case c(_) + f(_, r) => r("from c"); c() },
      go { case d(_) + f2(_, r) => r("from d") }
    )

    c()

    val givenFuture = Future {
      Thread.sleep(20)
    } // waiter has 150 ms timeout

    (givenFuture & e()).map { _ => waiter.dismiss() }
    // The test would fail if e() were emitted right away at this point.

    f() shouldEqual "from c"
    waiter.await()
    f2() shouldEqual "from d"

  }

  behavior of "#moleculeFuture"

  it should "create a future that succeeds when molecule is emitted" in {
    val waiter = new Waiter
    val tp = FixedPool(4)
    implicit val _ = tp.executionContext

    val b = m[Unit]

    // "fut" will succeed when "c" is emitted
    val (c, fut) = moleculeFuture[String](tp)

    site(tp)(
      go { case b(_) => c("send it off") }
    )

    for {
      s <- fut
    } yield {
      waiter {
        s shouldEqual "send it off"
        ()
      }
      waiter.dismiss()
    }

    b()
    waiter.await()(patienceConfig, implicitly[Position])

  }

  behavior of "litmus"

  it should "create two molecule emitters" in {
    withPool(FixedPool(4)) { tp ⇒
      val (carrier, fetch) = litmus[Long](tp)
      carrier.name shouldEqual "carrier"
      fetch.name shouldEqual "fetch"

      fetch.timeout()(1.seconds) shouldEqual None

      carrier(123L)
      fetch() shouldEqual 123L
    }.get
  }

  behavior of "cleanup with resource"

  it should "catch exceptions and not fail" in {
    val tryX = cleanup(1)(_ => throw new Exception("ignore this exception"))(_ => throw new Exception("foo"))
    tryX.isFailure shouldEqual true
  }

}
