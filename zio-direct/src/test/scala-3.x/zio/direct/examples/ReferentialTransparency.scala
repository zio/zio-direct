package zio.direct.examples

object ReferentialTransparency {

  def defer[T](t: T): List[T] = ???
  def run[T](list: List[T]): T = ???

  implicit class ListOps[T](list: List[T]) {
    def run: T = ???
  }

  defer {
    val int = run(List(1, 2))
    val bool = run(List(true, false))
    (int, bool)
  }
// List((1, true), (1, false), (2, true), (2, false))

  for {
    int <- List(1, 2)
    bool <- List(true, false)
  } yield (int, bool)

  List(1, 2).flatMap { int =>
    List(true, false).map { bool =>
      (int, bool)
    }
  }

  defer {
    val bool = run(List(true, false))
    val int = run(List(1, 2))
    (int, bool)
  }
  // List((1, true), (2, true), (1, false), (2, false))
  List(true, false).flatMap { int =>
    List(1, 2).map { bool =>
      (int, bool)
    }
  }

  for {
    bool <- List(true, false)
    int <- List(1, 2)
  } yield (int, bool)

defer {
  val int = List(1, 2).run
  val bool = List(true, false).run
  val bool1 = bool
  val int1 = int
  (int, bool)
}

defer {
  val bool = List(true, false).run
  val int = List(1, 2).run
  val bool1 = bool
  val int1 = int
  (int, bool)
}

}
