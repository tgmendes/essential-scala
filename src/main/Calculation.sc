object main {

  sealed trait Calculation

  final case class Success(result: Int) extends Calculation

  final case class Failure(reason: String) extends Calculation

  object Calculator {
    def +(calc: Calculation, op: Int): Calculation = calc match {
      case Success(res) => Success(res + op)
      case Failure(reason) => Failure(reason)
    }

    def -(calc: Calculation, op: Int): Calculation = calc match {
      case Success(res) => Success(res - op)
      case Failure(reason) => Failure(reason)
    }
  }
}

assert(main.Calculator.+(main.Success(1), 1) == main.Success(2))
assert(main.Calculator.-(main.Success(1), 1) == main.Success(0))
assert(main.Calculator.+(main.Failure("Badness"), 1) == main.Failure("Badness"))
