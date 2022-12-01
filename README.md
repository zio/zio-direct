# ZIO direct

| Project Stage | CI | Release | Snapshot | Discord |
| --- | --- | --- | --- | --- |
| [![Project stage][Badge-Stage]][Link-Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Snapshot Artifacts][Badge-SonatypeSnapshots]][Link-SonatypeSnapshots] | [![Badge-Discord]][Link-Discord] |

# Summary
Direct-Style programming in ZIO based on the Monadless paradigm.


# Documentation
[ZIO Direct Microsite](https://zio.github.io/zio-direct/)

ZIO-Direct allows direct style programming with ZIO. This library provides a *syntactic sugar* that is more powerful than for-comprehensions as well as more natural to use. Simply add the `.run` suffix to any ZIO effect in order to retrieve it's value.

ZIO-Direct works by using by using macros to rewrite sequential code into flatMap-chains. The values resulting in `.run` calls from the ZIO effects are not actually awaited. Instead, they are rolled-up into a chain of flatMaps.

For example, in imperative programming operations typically are done in a simple set of steps.
```scala
object FileOps:
  def read(file: File): String
  def write(file: File, content: String): Unit

val textA = read(fileA)
val textB = read(fileB)
write(fileC, textA + textB)
```

Using functional programming, the equivalent of this functionality is a set of nested flatMap-chains.
```scala
object FileOps
  def read(file: File): ZIO[Any, Throwable, String]
  def write(file: File, content: String): ZIO[Any, Throwable, Unit]

read(fileA).flatMap { textA =>
  read(fileB).flatMap { textB =>
    write(fileC, textA + textB)
  }
}
```

In order to avoid this complexity scala provides a for-comprehension syntactic sugar.
```scala
for {
  textA <- read(fileA)
  textB <- read(fileB)
  _ <- write(fileC, textA + textB)
} yield ()
```

Unfortunately this syntactic sugar is limited in many cases, for example, inserting a conditional value inside is impossible.
```scala
for {
  textA <- read(fileA)
  // Not a possible syntax
  if (fileA.contains("some string")) {
    textB <- read(fileB)
    _ <- write(fileC, textA + textB)
  }
} yield ()
```

ZIO-Direct offers an equivalent syntactic sugar that is more ergonomic and allows many constructs that for-comprehensions do not.
```scala
defer {
  val textA = read(fileA).run
  if (fileA.contains("some string")) {
    val textB = read(fileB).run
    write(fileC, textA + textB).run
  }
}
```


# ZIO-Tailored
ZIO-Direct is specifically tailored to ZIO capabilities as it supports Environment and Error composition in ZIO effects similar to the for-comprehension.

```scala
val out: ZIO[CustomerConfig & DistributorConfig, CustomerGetException | DistrubutorGetException, (Customer, Distributor)] =
  defer {
    // Get a customer-configuration object from the environment and extract its .url field
    val custUrl: String = ZIO.service[CustomerConfig].run.url
    // Get a distributor-configuration from the environment and extract its .url field
    val distUrl: String = ZIO.service[DistributorConfig].run.url
    (
      // Use the two configurations to make an HTTP-call
      parseCustomer(httpGetCustomer(custUrl).run),
      parseDistrubutor(httpGetDistributor(distUrl).run)
    )
  }
```

# Branching and Looping Support
Unlike the for-comprehension, ZIO-Direct supports branching and looping in the use of flatMaps composition.
Let's have a look at a another non-trivial example.

```scala
class Database:
  def nextRow(): ZIO[Any, Throwable, Row]
  def hasNextRow(): ZIO[Any, Throwable, Boolean]
  def lockNextRow(): ZIO[Any, Throwable, Boolean]
object Database:
  def open: ZIO[Any, Throwable, Database]

defer {
  // Open a database connection
  val db = Database.open().run
  // See if there is is a next-row
  while (db.hasNextRow().run) {
    // try to lock, if aquired continue
    if (db.lockNextRow().run)
      val nextRow = db.nextRow().run
      doSomethingWith(nextRow)
    else
      waitT()
  }
}
```
> NOTE: The above database-api is imaginary.

The above code needs to be translated into something like this:

```scala
Database.open.flatMap { db =>
  def whileFun(): ZIO[Any, Throwable, Unit] =
    db.hasNextRow().flatMap { hasNextRow =>
      if (hasNextRow)(
        db.lockNextRow().flatMap { lockNextRow =>
          if (!lockNextRow)
            db.nextRow().map(nextRow => doSomethingWith(nextRow))
          else
            ZIO.succeed(waitT())
        }
      ).flatMap(_ => whileFun())
      else
        ZIO.unit
    }
  whileFun()
}
```

Note that normally this is the exact code that would have to be written to capture such functionality For-comprehensions do not provide a way to do looping and branching so in such cases
a combination of flatMaps and recursion is necessary to avoid calling effects unnecessarily.


## Code of Conduct

See the [Code of Conduct](https://zio.github.io/zio-direct/docs/about/about_coc)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].


# License
[License](LICENSE)

[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-direct_2.12.svg "Sonatype Releases"
[Badge-SonatypeSnapshots]: https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-direct_2.12.svg "Sonatype Snapshots"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Badge-CI]: https://github.com/zio/zio-direct/workflows/CI/badge.svg
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-direct_2.12/ "Sonatype Releases"
[Link-SonatypeSnapshots]: https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-direct_2.12/ "Sonatype Snapshots"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Badge-Stage]: https://img.shields.io/badge/Project%20Stage-Development-yellowgreen.svg
[Link-Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
