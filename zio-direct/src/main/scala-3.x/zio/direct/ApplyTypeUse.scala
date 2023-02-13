// package zio.direct

// import zio.ZIOAppDefault
// import zio.Scope
// import zio.ZIO
// import zio.ZIOAppArgs
// import zio.ZLayer
// import java.time.LocalDate

// object ApplyTypeUse extends ZIOAppDefault {
//   override def run =
//     ApplyTypeExample[ZIO](
//       (for {
//           str <- ZIO.service[String]
//           _ <- ZIO.succeed(println(s"ZIO SHOWING: $str"))
//         } yield ()
//         // Let's pretend we've lost the type information...
//         // and let the transparent macro figure it out again!
//       ).asInstanceOf[Any]
//     ).provide(ZLayer.succeed("foobar"))
// }

// object MoreThan3Use {
//   class FourUse[A, B, C, D] {
//     def addA(a: A) = println(s"added A: $a")
//     def addB(b: B) = println(s"added B: $b")
//     def addC(c: C) = println(s"added C: $c")
//     def addD(d: D) = println(s"added D: $d")
//   }

//   type FourUseTo3[A, B, C] = FourUse[A, B, C, LocalDate]
//   def main(args: Array[String]): Unit = {
//     // [A, B, C] =>> FourUse[A, B, C, LocalDate]
//     val ret = ApplyTypeExample[FourUseTo3](
//       new FourUse[Any, Any, Any, Any]().asInstanceOf[Any]
//     )
//     ret.addA("foo")
//     ret.addD(LocalDate.now())
//   }
// }
