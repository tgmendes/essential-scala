object main {
  sealed trait Result[A]
  case class Success[A](result: A) extends Result[A]
  case class Failure[A](reason: String) extends Result[A]

  sealed trait LinkedList[A] {
    def fold[B](end: B, pair: (A, B) => B): B = this match {
      case End() => end
      case Pair(hd, tl) => pair(hd, tl.fold(end, pair))
    }

    def apply(index: Int): Result[A] =
      fold[Result[Int]](Failure("Index our of bounds"), (hd, tl) =>
       if(index == 0) Success(hd) else index-1)

//
//      this match {
//
//      case Pair(hd, tl) =>
//        if(index  == 0) Success(hd) else tl(index-1)
//      case End() => Failure("Index out of bounds")
//    }

    def length: Int =
      this match {
        case End() => 0
        case Pair(hd, tl) => 1 + tl.length
      }

    //    def double: LinkedList[A] =
    //      this match {
    //        case End() => End()
    //        case Pair(hd, tl) => Pair(hd * 2, tl.double)
    //      }

    //    def product: A =
    //      this match {
    //        case End() => 1
    //        case Pair(hd, tl) => hd * tl.product
    //      }
    //
    //    def sum: A =
    //      this match {
    //        case End() => 0
    //        case Pair(hd, tl) => hd + tl.sum
    //      }
    def contains(item: A): Boolean = this match {
      case Pair(hd, tl) => if (hd == item) true else tl.contains(item)
      case End() => false
    }
  }

  final case class End[A]() extends LinkedList[A]

  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]

  val example = Pair(1, Pair(2, Pair(3, End())))

  // Generic Types 1
  assert(example.length == 3)
  assert(example.tail.length == 2)
  assert(End().length == 0)

  // Generic Types 2
  assert(example.contains(3))
  assert(!example.contains(4))
  assert(!End().contains(0))

  // Generic Types 3
  assert(example(0) == 1)
  assert(example(1) == 2)
  assert(example(2) == 3)
  assert(example(3) == Failure("Index out of bounds"))
}

main

