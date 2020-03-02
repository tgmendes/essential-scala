object m {

  sealed trait IntList {
    def length: Int =
      this match {
        case End => 0
        case Pair(_, tl) => 1 + tl.length
      }

    def product: Int =
      this match {
        case End => 1
        case Pair(hd, tl) => hd * tl.product
      }

    def double: IntList =
      this match {
        case End => End
        case Pair(hd, tl) => Pair(hd * 2, tl.double)
      }
  }

  case object End extends IntList

  final case class Pair(head: Int, tail: IntList) extends IntList

  def sum(list: IntList): Int =
    list match {
      case End => 0
      case Pair(hd, tl) => hd + sum(tl)
    }
}

val example = m.Pair(1, m.Pair(2, m.Pair(3, m.End)))

// Sum
assert(m.sum(example) == 6)
assert(m.sum(example.tail) == 5)
assert(m.sum(m.End) == 0)

// Length
assert(example.length == 3)
assert(example.tail.length == 2)
assert(m.End.length == 0)

// Product
assert(example.product == 6)
assert(example.tail.product == 6)
assert(m.End.product == 1)

// Double
assert(example.double == m.Pair(2, m.Pair(4, m.Pair(6, m.End))))
assert(example.tail.double == m.Pair(4, m.Pair(6, m.End)))
assert(m.End.double == m.End)