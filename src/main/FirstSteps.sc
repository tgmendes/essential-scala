class Adder(amount: Int) {
  def apply(in: Int) = in + amount
}

class Counter(val count: Int) {
  def dec: Counter = dec()
  def inc: Counter = inc()
  def inc(step: Int = 1): Counter = new Counter(count + step)
  def dec(step: Int = 1): Counter = new Counter(count - step)
  def adjust(adder: Adder): Counter = new Counter(adder(count))
}

val adder = new Adder(10)
adder(2)
val counter = new Counter(10)
counter.adjust(adder).count

