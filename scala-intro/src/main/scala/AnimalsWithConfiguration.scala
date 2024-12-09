object AnimalsWithConfiguration {

  // Gleich wie Animals, nur dass noch 
  // die Animal-Configuration enthalten ist, und die runOver-Methode Rücksicht auf eine AnimalConfiguration nimmt
  
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

    def runOver(using animalConf: AnimalConfiguration): Animal =
      this match {
        case Dillo(liveness, weight) =>
          if (animalConf.speed > 10) {
            Dillo(Liveness.Dead, weight)
          } else {
            Dillo(liveness, weight)
          }

        case Parrot(liveness, weight, sentence) =>
          if (animalConf.speed > 50 && animalConf.isSlippery) {
            Parrot(Liveness.Dead, weight, "")
          } else {
            Parrot(liveness, weight, sentence)
          }
      }
  }

  val dillo1 = Animal.Dillo(Liveness.Alive, 10)
  
  case class AnimalConfiguration(speed: Double, isSlippery: Boolean)
  val animalConf = AnimalConfiguration(55, true)
  
  given AnimalConfiguration = AnimalConfiguration(55, true)


  dillo1.runOver
  dillo1.runOver(using AnimalConfiguration(42, false))



}
