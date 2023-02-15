package zio.direct

import zio.ZIO

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel] {
  inline def success = ZioMonad.Success
  inline def fallible = ZioMonad.Fallible
  inline def sequence = ZioMonad.Sequence
  inline def sequencePar = ZioMonad.SequenceParallel
  inline def state = FailUnused.forMonadState()
  inline def log = FailUnused.forMonadLog()
}
