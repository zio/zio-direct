package zio.direct.examples

object FutureDirections {
  class defer[F[_]] {
    def from[T](t: T): F[T] = ???
  }

  // object defer[F[_]] {
  //   def apply[F[_]]()
  // }
  extension [F[_], T](stmt: F[T])
    def run: T = ???

  {
    case class Person(id: Int)
    case class Address(street: String)
    class Future[T]

    def httpGet[T](str: String): Future[T] = ???

    val q: Future[(Person, Address)] =
      defer[Future].from {
        val p = httpGet[Person]("http://people").run
        val a = httpGet[Address](s"http://address?owner=${p.id}").run
        (p, a)
      }
  }

  {
    case class Person(id: Int, addresses: List[Address])
    case class Address(street: String)
    class Future[T]

    def people: List[Person] = ???

    val q: List[(Person, Address)] =
      defer[List].from {
        val p = people.run
        val a = p.addresses.run
        (p, a)
      }
  }

  {
    trait Query[T] {
      def join(f: T => Boolean): Query[T] = ???
    }
    def query[T]: Query[T] = ???
    case class Person(id: Int, addresses: List[Address])
    case class Address(fk: Int, street: String)
    class Future[T]

    val q: Query[(Person, Address)] =
      defer[Query].from {
        val p = query[Person].run
        val a = query[Address].join(a => a.fk == p.id).run
        (p, a)
      }
  }
}
