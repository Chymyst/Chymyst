package io.chymyst

import io.chymyst.jc.{+, AllMatchersAreTrivial, B, InputMoleculeInfo, M, Pool, Reaction, ReactionInfo, b, defaultReactionPool, go, m, site}

import scala.concurrent.{ExecutionContext, Future, Promise}


package object lab {
  /** Create a non-blocking molecule that, when emitted, will resolve the future.
    * Example usage: val (m, fut) = moleculeFuture[String](pool)
    *
    * @param pool Thread pool on which to run the new reaction site.
    * @tparam T Type of value carried by the molecule and by the future.
    * @return Tuple consisting of new molecule emitter and the new future.
    */
  def moleculeFuture[T](pool: Pool = defaultReactionPool): (M[T], Future[T]) = {
    val f = new M[T]("future")
    val p = Promise[T]()

    site(pool)(
      go { case f(x) => p.success(x); () }
    )
    (f, p.future)
  }

  implicit class FutureWithMolecule[T](f: Future[T])(implicit ec: ExecutionContext) {
    /** Modify the future: when it succeeds, it will additionally emit a given molecule.
      * The value on the molecule will be equal to the result value of the future.
      * (The result value of the future is unchanged.)
      *
      * Example usage: Future { ... } & a
      *
      * @param m Molecule emitter, must have the same type as the future.
      * @return The modified future.
      */
    def &(m: M[T]): Future[T] = f.map { x =>
      m(x)
      x
    }(ec)

    /** Modify the future: when it succeeds, it will additionally emit a given molecule.
      * The molecule will carry the specified value (the result value of the future is unchanged).
      *
      * Example usage: Future { ... } + a(123)
      *
      * @param u Molecule emission expression, such as a(123)
      * @return The modified future.
      */
    def +(u: => Unit): Future[T] = f.map { x =>
      u
      x
    }(ec)
  }

  def litmus[T](pool: Pool): (M[T], B[Unit, T]) = {
    val carrier = m[T]
    val fetch = b[Unit, T]
    site(pool)(
      go { case carrier(x) + fetch(_, r) ⇒ r(x) }
    )
    (carrier, fetch)
  }
}
