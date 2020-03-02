object main {
  sealed trait Sum[A, B]
  final case class Left[A, B](value: A) extends Sum[A, B]
  final case class Right[A, B](value: B) extends Sum[A, B]

  println(Left[Int, String](1).value)
  println(Right[Int, String]("foo").value)
  val sum: Sum[Int, String] = Right("foo")
  sum match {
    case Left(x) => x.toString
    case Right(x) => x
  }
}
main