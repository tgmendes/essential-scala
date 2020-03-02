case class Cat(name: String, color: String, food: String) {}

val c1 = new Cat("Whiskas", "Black", "Milk")
val c2 = new Cat("Tareco", "Ginger", "Chips")
val c3 = new Cat("Saketas", "Tabby and white", "Curry")

println(c1)
println(c2)

object ChipShop {
  def willServe(cat: Cat): Boolean =
    cat match {
      case Cat(_, _, "Chips") => true
      case Cat(_, _, _) => false
    }
}

ChipShop.willServe(c1)
ChipShop.willServe(c2)
ChipShop.willServe(c3)