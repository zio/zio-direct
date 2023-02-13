package zio.direct

import zio.ZIO

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel] {
  transparent inline def success = ZioMonad.Success
  transparent inline def fallible = Some(ZioMonad.Fallible)
  transparent inline def sequence = ZioMonad.Sequence
  transparent inline def sequencePar = ZioMonad.SequenceParallel
  transparent inline def state = None
  transparent inline def log = None
}
