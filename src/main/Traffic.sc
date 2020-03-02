
object main {
  sealed trait TrafficLight {
    def next(): TrafficLight
  }
  case object Red extends TrafficLight {
    def next(): TrafficLight = Green
  }
  case object Green extends TrafficLight {
    def next(): TrafficLight = Yellow
  }
  case object Yellow extends TrafficLight {
    def next(): TrafficLight = Red
  }
}

main.Red.next().next().next()

object mainMatching {
  sealed trait TrafficLight {
    def next(): TrafficLight = this match {
      case Red => Green
      case Yellow => Red
      case Green => Yellow
    }

  }

  final case object Red extends TrafficLight

  final case object Green extends TrafficLight

  final case object Yellow extends TrafficLight
}

mainMatching.Red.next().next().next()