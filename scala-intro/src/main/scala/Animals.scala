object Animals {
  type Weight = Double
  enum Liveness {
    case Alive
    case Dead
  }

  enum Animal {
    // Ein Gürteltier hat eine Lebendigkeit und ein Gewicht
    case Dillo(liveness: Liveness, weight: Weight)
    // Ein Papagei hat eine Lebendigkeit, ein Gewicht, und kann einen Satz sagen und hat ein Gewicht
    case Parrot(liveness: Liveness, weight: Weight, sentence: String)

    def runOver: Animal =
      this match {
        case Dillo(liveness, weight) =>
          Dillo(Liveness.Dead, weight)
        case Parrot(liveness, weight, sentence) =>
          Parrot(Liveness.Dead, weight, "")
      }
  }

  val dillo1 = Animal.Dillo(Liveness.Alive, 10)
  val parrot1 = Animal.Parrot(Liveness.Alive, 1, "Ich bin ein Plapperpapagei")

  import Animal._ // Alles aus Animal importieren

  val dillo2 = Dillo(Liveness.Alive, 2)
  val parrot2 = Parrot(Liveness.Alive, 1.5, "hasta la vista, baby")

  val highway = Seq(dillo1, dillo2, parrot1, parrot2)
  val highway1: Seq[Animal] = Seq(dillo1, dillo2, parrot1, parrot2)

  
  def runOverAnimals_old1(animals: Seq[Animal]): Seq[Animal] =
    animals match {
      case Nil => Nil
      case head +: rest => head.runOver +: runOverAnimals(rest)
    }
  

  def runOverAnimals(animals: Seq[Animal]): Seq[Animal] =
    animals.map(a => a.runOver)

  runOverAnimals(highway1)

  extension (animals: Seq[Animal]) {
    def runOver = animals.map(_.runOver)
  }

  highway1.runOver

}