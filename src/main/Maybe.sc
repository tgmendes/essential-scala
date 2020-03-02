sealed trait Maybe[A] {
  def fold[B](full: A => B, empty: B): B =
    this match {
      case Full(v) => full(v)
      case Empty() => empty
    }

  def map[B](fn: A => B): Maybe[B] =
    this match {
      case Full(value) => Full(fn(v))
      case Empty() => Empty[B]()
    }
  def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
    this match {
      case Full(v) => fn(v)
      case Empty() => Empty[B]()
    }

}
final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

val perhaps: Maybe[Int] = Empty[Int]
val perhaps: Maybe[Int] = Full(1)
