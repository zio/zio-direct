package zio.direct.list

import zio.direct.deferCall
import zio.direct.directRunCall
import zio.direct.core.NotDeferredException

/**
 * Turns a block of code into a `List` of all the results. Unlike the other DSLs, this entry point is named
 * `select`, not `defer`. Inside the block, `from(xs)` takes each element of `xs` in turn, so the rest of the
 * block runs once for each element.
 *
 * {{{
 * import zio.direct.list.*
 *
 * val pairs = select {
 *   val a = from(List(1, 2))
 *   val b = from(List("x", "y"))
 *   (a, b)
 * }
 * }}}
 */
object select extends deferCall[[R, E, A] =>> List[A], List[?], Nothing, Nothing, ListMonadModel] {

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def success = listMonadSuccess

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def fallible = None

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def sequence = listMonadSequence

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def sequencePar = listMonadSequencePar

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def state = None

  /**
   * Monad hook for `List`. See `deferCall`.
   */
  transparent inline def log = None
}

extension [A](value: List[A]) {

  /**
   * Takes each element of this list in turn inside a `select { ... }` block. Outside one, it throws
   * `NotDeferredException`.
   */
  @directRunCall
  def from: A = NotDeferredException.fromNamed("from")
}
