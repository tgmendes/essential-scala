sealed trait TrafficLight {
  def Next: TrafficLight
}


case object Red extends TrafficLight {
  def Next: TrafficLight = Green
}

case object Yellow extends TrafficLight {
  def Next: TrafficLight = Red
}


case object Green extends TrafficLight {
  def Next: TrafficLight = Red
}





