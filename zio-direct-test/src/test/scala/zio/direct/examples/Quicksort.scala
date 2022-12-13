// package zio.direct.examples

// import zio._
// import zio.direct._

// object Quicksort {

//   def quicksortDefer(arr: Array[Int]): ZIO[Any, Nothing, Unit] = {
//     def swap(i: Int, j: Int) = {
//       val temp = arr(i)
//       arr(i) = arr(j)
//       arr(j) = temp
//     }

//     def sort(l: Int, r: Int): ZIO[Any, Nothing, Unit] = defer(Use.withLenientCheck) {
//       val pivot = arr((l + r) / 2)
//       val i = Ref.make(l).run
//       val j = Ref.make(r).run
//       while (i.get.run <= j.get.run) {
//         while (arr(i.get.run) < pivot) i.getAndUpdate(i => i + 1).run
//         while (arr(j.get.run) > pivot) j.getAndUpdate(j => j - 1).run
//         if (i.get.run <= j.get.run) {
//           swap(i.get.run, j.get.run)
//           i.getAndUpdate(i => i + 1).run
//           j.getAndUpdate(j => j - 1).run
//         }
//       }

//       if (l < j.get.run) {
//         val jv = j.get.run
//         sort(l, jv).run
//       }
//       if (j.get.run < r) {
//         val iv = i.get.run
//         sort(iv, r).run
//       }
//     }
//     sort(0, arr.length - 1)
//   }

//   def quicksortImperative(a: Array[Int]): Unit = {
//     def swap(i: Int, j: Int): Unit = {
//       val t = a(i)
//       a(i) = a(j)
//       a(j) = t
//     }
//     def sort(l: Int, r: Int): Unit = {
//       val pivot = a((l + r) / 2)
//       var i = l
//       var j = r
//       while (i <= j) {
//         while (a(i) < pivot) i += 1
//         while (a(j) > pivot) j -= 1
//         if (i <= j) {
//           swap(i, j)
//           i += 1
//           j -= 1
//         }
//       }
//       if (l < j) sort(l, j)
//       if (j < r) sort(i, r)
//     }
//     sort(0, a.length - 1)
//   }

//   def main(args: Array[String]): Unit = {
//     val arr = Array(3, 2, 8, 5, 7, 2, 3, 8, 9, 4, 5, 8, 2, 3, 4, 7, 6, 5, 9, 2, 3, 8, 4, 7, 5, 6, 2, 0, 8, 3)
//     RunNow(quicksortDefer(arr))
//     println(arr.toList)
//   }
// }
