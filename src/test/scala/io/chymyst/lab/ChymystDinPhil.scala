package io.chymyst.lab

import io.chymyst.jc._
import org.scalatest.{FlatSpec, Matchers}

class ChymystDinPhil extends FlatSpec with Matchers {

  /** Print message and wait for a random time interval. */
  def wait(message: String): Unit = {
    println(message)
    Thread.sleep(scala.util.Random.nextInt(20).toLong)
  }

  behavior of "the `dining philosophers` problem"

  it should "run simple solution with 5 philosophers" in {

    val h1 = m[Unit]
    val h2 = m[Unit]
    val h3 = m[Unit]
    val h4 = m[Unit]
    val h5 = m[Unit]

    val t1 = m[Unit]
    val t2 = m[Unit]
    val t3 = m[Unit]
    val t4 = m[Unit]
    val t5 = m[Unit]

    val fork12 = m[Unit]
    val fork23 = m[Unit]
    val fork34 = m[Unit]
    val fork45 = m[Unit]
    val fork51 = m[Unit]

    site(
      go { case t1(_) ⇒ wait("Socrates is thinking"); h1() },
      go { case t2(_) ⇒ wait("Confucius is thinking"); h2() },
      go { case t3(_) ⇒ wait("Plato is thinking"); h3() },
      go { case t4(_) ⇒ wait("Descartes is thinking"); h4() },
      go { case t5(_) ⇒ wait("Voltaire is thinking"); h5() },

      // To the right of h1 is h12, to the left is fork51
      go { case h1(_) + fork12(_) + fork51(_) ⇒
        wait("Socrates is eating"); t1() + fork12() + fork51()
      },

      go { case h2(_) + fork23(_) + fork12(_) ⇒
        wait("Confucius is eating"); t2() + fork23() + fork12()
      },

      go { case h3(_) + fork34(_) + fork23(_) ⇒
        wait("Plato is eating"); t3() + fork34() + fork23()
      },

      go { case h4(_) + fork45(_) + fork34(_) ⇒
        wait("Descartes is eating"); t4() + fork45() + fork34()
      },

      go { case h5(_) + fork51(_) + fork45(_) ⇒
        wait("Voltaire is eating"); t5() + fork51() + fork45()
      }
    )
    // Emit molecules representing the initial state:
    t1() + t2() + t3() + t4() + t5()
    fork12() + fork23() + fork34() + fork45() + fork51()
    // Now reactions will start and print messages to the console.
    Thread.sleep(400)
  }

  it should "simulate n philosophers using dynamically defined reactions" in {
    def run_philosophers(n: Int): Unit = {
      // Create lists of philosopher and fork molecules.
      val indices = 0 until n
      val ts = indices.map { i ⇒ new M[Unit](s"t$n") }
      val hs = indices.map { i ⇒ new M[Unit](s"h$n") }
      // `fs` is the sequence of forks to the left of philosophers.
      // E.g. (fork9:0, fork0:1, ..., fork8:9).
      val fs = indices.map { i ⇒
        val prev = (i + n - 1) % n
        new M[Unit](s"fork$prev:$i") // e.g. fork4:5 is the fork between 4 and 5.
      }
      // Compute the sequence of forks to the right, for convenience:
      val fs_right = fs.tail :+ fs.head // E.g. (fork0:1, ..., fork8:9, fork9:0).

      // Create reactions for thinking.
      // E.g. go { case t4(_) ⇒ wait("Philosopher 4 is thinking");  h4() }
      val thinking_reactions = indices.map { i ⇒
        // Molecule emitters need to be made into local variables.
        val t = ts(i)
        val h = hs(i)
        go { case t(_) ⇒ wait(s"Philosopher $i is thinking"); h() }
      }
      // Create reactions for eating.
      // E.g. go { case h4(_) + fork45(_) + fork34(_) ⇒
      //  wait("Philosopher 4 is eating"); t4() + fork45() + fork34() }
      val eating_reactions = indices.map { i ⇒
        val t = ts(i)
        val h = hs(i)
        val f_left = fs(i)
        val f_right = fs_right(i)
        go { case h(_) + f_left(_) + f_right(_) ⇒
          wait(s"Philosopher $i is eating"); t() + f_left() + f_right()
        }
      }

      // Create a reaction site.
      site(eating_reactions ++ thinking_reactions :_*)

      // Emit the initial molecules.
      ts.foreach(_.apply())
      fs.foreach(_.apply())

    }

    run_philosophers(10)
    Thread.sleep(400)
  }
}
