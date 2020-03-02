sealed trait Color {
  def red: Int
  def green: Int
  def blue: Int

  def lightness(): String = {
    val hsp = math.sqrt(0.299 * (red * red) + 0.587 * (green * green) + 0.114 * (blue * blue))
    if (hsp > 127.5) "light" else "dark"
  }
}

case class Red() extends Color {
  val red = 227
  val green = 38
  val blue = 25
}

case class Yellow() extends Color {
  val red = 250
  val green = 239
  val blue = 25
}

case class Pink() extends Color {
  val red = 242
  val green = 25
  val blue = 250
}

case class CustomColor(
    val red: Int,
    val green: Int,
    val blue: Int
) extends Color

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Color
}

sealed trait Rectangular extends Shape {
  def width: Double
  def height: Double

  val sides = 4
  override val perimeter = 2*width + 2*height
  override val area = width * height
}

case class Circle(radius: Double, color: Color) extends Shape {
  val sides = 1
  val perimeter = 2 * math.Pi * radius
  val area = math.Pi * radius * radius
}

case class Square(size: Double, color: Color) extends Rectangular {
  val width = size
  val height = size
}

case class Rectangle(val width: Double, val height: Double, color: Color) extends Rectangular


object Draw {
  def apply(shape: Shape): String = shape match {
    case Rectangle(width, height, color) =>
      s"A ${colorType(color)} rectangle of width ${width}cm and height ${height}cm"
    case Square(size, color) =>
      s"A ${colorType(color)} square of size ${size}cm"
    case Circle(radius, color) =>
      s"A ${colorType(color)} circle of radius ${radius}cm"
  }

  def colorType(color: Color): String = color match {
    case Red() =>
      "red"
    case Yellow() =>
      "yellow"
    case Pink() =>
      "pink"
    case CustomColor(_, _, _) =>
      color.lightness()
  }
}

val sq = Square(2.5, Red())
val rec = Rectangle(2.5, 5.3, Pink())
val circ = Circle(3.14, CustomColor(200, 20, 60))


println(sq.perimeter)
println(sq.area)
println(sq.sides)

println(rec.perimeter)
println(rec.area)

Draw(rec)
Draw(sq)
Draw(circ)