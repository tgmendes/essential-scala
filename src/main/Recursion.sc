object main {

  sealed trait LinkedList[A] {
    def fold(end: Int, f: (Int, Int) => Int): Int =
      this match {
        case End() => end
        case Pair(hd, tl) => f(hd, tl.fold(end, f))
      }

    def length: Int = {
      this match {
        case Pair(hd, tl) => 1 + tl.length
        case End() => 0
      }
    }

    def contains(item: A): Boolean = {
      this match {
        case Pair(hd, tl) =>
          if(hd == item)
            true
          else
            tl.contains(item)

        case End() => false
      }
    }

    def apply(index: Int): A =
      this match {
        case Pair(hd, tl) =>
          if(index == 0)
            hd
          else
            tl(index - 1) case End() =>
          throw new Exception("Attempted to get element from an Empty list")
            }

//    def double: LinkedList[A] =
//      this match {
//        case End() => End
//        case Pair(hd, tl) => Pair(hd * 2, tl.double)
//      }
//    def product: Int =
//      this match {
//        case End() => 1
//        case Pair(hd, tl) => hd * tl.product
//      }
//    def sum: Int =
//      this match {
//        case End() => 0
//        case Pair(hd, tl) => hd + tl.sum
//
//      }
  }

  final case class End extends LinkedList

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]


}


val example = main.Pair(1, main.Pair(2, main.Pair(3, main.End())))

assert(example.length == 3)
assert(example.tail.length == 2)
assert(main.End().length == 0)

assert(example.contains(3))
assert(example.contains(2))
//assert(main.End().contains(0))

assert(example(0) == 1)
assert(example(1) == 2)
assert(example(2) == 3)
assert(try {
  example(3)
  false
} catch {
  case e: Exception => true
})
println("end")
