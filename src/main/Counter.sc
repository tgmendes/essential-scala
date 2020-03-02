class Adder(amount: Int) {
  def apply(in: Int) = in + amount
}

case class Counter(count: Int) {
  def dec = copy(count = count - 1)
  def inc = copy(count = count + 1)
//  def dec: Counter = dec()
//  def inc: Counter = inc()
//  def inc(step: Int = 1): Counter = new Counter(count + step)
//  def dec(step: Int = 1): Counter = new Counter(count - step)
  def adjust(adder: Adder): Counter = copy(adder(count))
}

val adder = new Adder(10)
adder(2)
val counter = new Counter(10)
counter.adjust(adder).count