object Exercises {
  trait Animal {
    def name: String
  }

  case class Cat(override val name: String) extends Animal

  case class Dog(override val name: String) extends Animal

  case class Shelter[+TAnimal <: Animal](animals: List[TAnimal]){
    def +[T >: TAnimal <: Animal] (animal: T) = new Shelter[T](this.animals :+ animal)
    def ++[T >: TAnimal <: Animal] (other: Shelter[T]) = new Shelter[T](this.animals ++ other.animals)
    def getNames: List[String] = animals.map(_.name)
    def feed(food: Food[TAnimal]): List[String] = animals.map(food.feed)
  }



  trait Food[-T <: Animal] {
    val foodName = ""
    def feed(animal: T) = s"${animal.name} eats $foodName"
  }

  case object Meat extends Food[Animal] {
    override val foodName: String = "meat"
  }

  case object Milk extends Food[Cat] {
    override val foodName: String = "milk"
  }

  case object Bread extends Food[Dog] {
    override val foodName: String = "bread"
  }
}
