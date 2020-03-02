sealed trait DivisionResult {

}

final case class Finite(result: Int) extends DivisionResult

case object Infinite extends DivisionResult

object divide {
  def apply(val1: Int, val2: Int): DivisionResult = {
    if (val2 == 0) Infinite else Finite(val1/val2)
  }
}

val x = divide(1, 2)
val y = divide(1, 0)