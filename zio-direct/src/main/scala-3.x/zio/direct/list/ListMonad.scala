package zio.direct.list

import zio.direct._
import zio.ZIO

type ThreeList[R, E, A] = List[A]

import MonadShape.Variance._
import MonadShape.Letter._
implicit val listMonadModel: MonadModel[ThreeList] {
  type Variances = MonadShape.Variances1[Covariant]
  type Letters = MonadShape.Letters1[A]
} = new MonadModel[ThreeList] {
  type Variances = MonadShape.Variances1[Covariant]
  type Letters = MonadShape.Letters1[A]
}

implicit inline def listMonadSuccess: MonadSuccess[ThreeList] = ListMonadSuccess

object ListMonadSuccess extends MonadSuccess[ThreeList] {
  inline def unit[A](a: => A): ThreeList[Any, Nothing, A] = List(a)
  inline def map[R, E, A, B](first: ThreeList[R, E, A])(andThen: A => B): ThreeList[R, E, B] = first.map[B](andThen)
  inline def flatMap[R, E, A, B](first: ThreeList[R, E, A])(andThen: A => ThreeList[R, E, B]): ThreeList[R, E, B] = first.flatMap[B](andThen)
  inline def flatten[R, E, A, R1 <: R, E1 >: E](first: ThreeList[R, E, ThreeList[R1, E1, A]]): ThreeList[R1, E1, A] = first.flatten
}

implicit inline def listMonadSequence: MonadSequence[ThreeList] = ListMonadSequence
object ListMonadSequence extends MonadSequence[ThreeList] {
  // This is really just an intersperse function. In many cases, A comes in as List[B] (same thing as ThreeList[R, E, B]) from
  // so the f is not even used often. It reduces to the intersperse function commented-out below.
  // I think Gen can also be used here something like this:
  // `gens.foldRight[Gen[R, List[A]]](Gen.const(List.empty))(_.zipWith(_)(_ :: _))`
  inline def traverseThreeList[R, E, A, B](as: List[A], f: A => ThreeList[R, E, B]): ThreeList[R, E, List[B]] = {
    as.foldLeft(List(List.empty[B]): ThreeList[R, E, List[B]])((accum: ThreeList[R, E, List[B]], a: A) => {
      val optB: ThreeList[R, E, B] = f(a)
      for {
        (list: List[B]) <- accum
        (b: B) <- optB
      } yield (list :+ b)
    })
  }

  // ------ The traverseThreeList usually just doing this: -------
  // inline def intersperse[B](as: List[List[B]]): List[List[B]] = {
  //   as.foldLeft(List(List.empty[B]): List[List[B]])((accum: List[List[B]], a: List[B]) => {
  //     for {
  //       (list: List[B]) <- accum
  //       (b: B) <- a
  //     } yield (list :+ b)
  //   })
  // }
  // ------ Which is really just this: -------
  // def crossN[B](as: List[List[B]]): List[R, E, List[B]] =
  //   as.foldLeft[List[List[A]]](ZStream.succeed(List.empty))(_.crossWith(_)(_ :+ _))

  inline def foreach[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ThreeList[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ThreeList[R, E, Collection[B]] =
    val traversed = traverseThreeList[R, E, A, B](in.toList, f)
    val out = traversed.map(list => bf.fromSpecific(in)(list))
    // println(s"==== IN: ${in}")
    // println(s"==== Traversed: ${traversed}")
    // println(s"==== Out: ${out}")
    out
}

implicit val listMonadSequencePar: MonadSequenceParallel[ThreeList] = new MonadSequenceParallel[ThreeList] {
  def foreachPar[R, E, A, B, Collection[+Element] <: Iterable[Element]](
      in: Collection[A]
  )(f: A => ThreeList[R, E, B])(implicit bf: scala.collection.BuildFrom[Collection[A], B, Collection[B]]): ThreeList[R, E, Collection[B]] =
    listMonadSequence.foreach(in)(f)
}
