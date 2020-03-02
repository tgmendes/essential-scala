
case class Director(firstName: String, lastName: String, yearOfBirth: Int) {
  def name(): String = s"$firstName $lastName"
}

object Director {
  def older(director1: Director, director2: Director): Director = if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
}

case class Film(
  val name: String,
  val yearOfRelease: Int,
  val imdbRating: Double,
  val director: Director) {

  def directorsAge(): Int = yearOfRelease - director.yearOfBirth

  def isDirectedBy(d: Director): Boolean =
    this.director == director
}

object Film {
  def newer(film1: Film, film2: Film): Film =
    if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

  def highestRating(film1: Film, film2: Film) =
    if(film1.imdbRating > film2.imdbRating) film1 else film2

  def oldestDirectorAtTheTime(film1: Film, film2: Film): Director =
    if(film1.directorsAge > film2.directorsAge) film1.director else film2.director
}



val eastwood = Director("Clint", "Eastwood", 1930)
val mcTiernan = Director("John", "McTiernan", 1951)
val nolan = Director("Christopher", "Nolan", 1970)
val someBody = Director("Just", "Some Body", 1990)
val memento = Film("Memento", 2000, 8.5, nolan)
val darkKnight = Film("Dark Knight", 2008, 9.0, nolan)
val inception = Film("Inception", 2010, 8.8, nolan)
val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7, eastwood)
val outlawJoseyWales = Film("The Outlaw Josey Wales", 1976, 7.9, eastwood)
val unforgiven = Film("Unforgiven", 1992, 8.3, eastwood)
val granTorino = Film("Gran Torino", 2008, 8.2, eastwood)
val invictus = Film("Invictus", 2009, 7.4, eastwood)
val predator = Film("Predator", 1987, 7.9, mcTiernan)
val dieHard = Film("Die Hard", 1988, 8.3, mcTiernan)
val huntForRedOctoberval = Film("The Hunt for Red October", 1990, 7.6, mcTiernan)
val thomasCrownAffair = Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

object Dad {
  def rate(film: Film): Double = {
    film match {
      case Film(_, _, _, Director("Clint", "Eastwood", _)) => 10.0
      case Film(_, _, _, Director("John", "McTiernan", _)) => 7.0
      case Film(_, _, _, _) => 3.0
    }
  }
}

Director.older(eastwood, mcTiernan).name
Film.highestRating(invictus, predator).name
Film.oldestDirectorAtTheTime(invictus, predator).name

eastwood.yearOfBirth
dieHard.director.name
invictus.isDirectedBy(nolan)

highPlainsDrifter.copy(name="L'homme des hautes plaines")

thomasCrownAffair.copy(yearOfRelease= 1968, director = new Director("Norman", "Jewison", 1926))

Dad.rate(invictus)
Dad.rate(predator)
Dad.rate(memento)