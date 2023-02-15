package zio.direct

import zio.ZIO

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel, Nothing] {
  def success = ZioMonad.Success
  def fallible = ZioMonad.Fallible
  def sequence = ZioMonad.Sequence
  def sequencePar = ZioMonad.SequenceParallel
  def state = ???
  def log = ???
}
