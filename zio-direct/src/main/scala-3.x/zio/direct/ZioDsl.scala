package zio.direct

import zio.ZIO

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel] {
  transparent inline def success = ZioMonad.zioMonadSuccess
  transparent inline def fallible = Some(ZioMonad.zioMonadFallible)
  transparent inline def sequence = ZioMonad.zioMonadSequence
  transparent inline def sequencePar = ZioMonad.zioMonadSequenceParallel
  transparent inline def state = None
  transparent inline def log = None
}
