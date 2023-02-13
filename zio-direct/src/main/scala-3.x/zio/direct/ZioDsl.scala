package zio.direct

import zio.ZIO

object defer extends deferCall[ZIO, ZIO[?, ?, ?], Nothing, Nothing, ZioMonad.ZioMonadModel](
      ZioMonad.zioMonadSuccess,
      Some(ZioMonad.zioMonadFallible),
      ZioMonad.zioMonadSequence,
      ZioMonad.zioMonadSequenceParallel,
      None,
      None
    )
