package lectures.part5ts

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  // what is variance?
  // Problem of "inheritance" in generics - type substitution of generics

  // Should a Cage[Cat] also "inherit" from Cage[Animal]?
  class Cage[T]
  // yes - covariance
  class CCage[+T]
  // Substituting a general cage o Animal with a cage o Cats
  val ccage: CCage[Animal] = new CCage[Cat]

  // no - invariance
  // Can't replace any type of cage with another type of cage
  class ICage[T]
  //  val icage: ICage[Animal] = ICage[Cat]
  //  val x: Int = "Hello World"

  // hell no -> opposite = contravariance
  class XCage[-T]
  // If a cage of animal can contain any animal, it can also contain a cat! Substitute the cat cage by a more generic cage
  val xCage: XCage[Cat] = new XCage[Animal]

  class InvariantCage[T](val animal: T) // invariant

  // covariant positions
  class CovariantCage[+T](
      val animal: T) // COVARIANT POSITION -> Field declaration -> Also accept invariant -> Look example above

  // class ContravariantCage[-T](val animal: T)
  /*
    If commented example above worked, we could do the following:

    val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)
   */

  // class CovariantVariableCage[+T](var animal: T) // types of vars are in CONTRAVARIANT POSITION -> var fields
  /*
    If commented example above worked, we could do the following:

    val ccage: CCAge[Animal] = new CCage[Cat](new Cat)
    ccage.animal = new Crocodile
   */

  // class ContravariantVariableCage[- T](var animal: T) // also in COVARIANT POSITION
  /*
    val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)
   */

  class InvariantVariableCage[T](var animal: T) // ok

  //  class anotherCovariantCage[+T] {
  //    def addAnimal(animal: T) // CONTRAVARIANT POSITION
  //  }

  /*
    val ccage: CCage[Animal] = new CCage[Dog]
    ccage.add(new Cat)
   */

  class AnotherContravariantCage[-T] {
    def addAnimal(animal: T) = true
  }

  val acc: AnotherContravariantCage[Cat] = new AnotherContravariantCage[Animal]
  acc.addAnimal(new Cat)
  class Kitty extends Cat
  acc.addAnimal(new Kitty)

  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = new MyList[B] // widening the type
  }

  val emptyList = new MyList[Kitty]
  val animals = emptyList.add(new Kitty)
  val moreAnimals = animals.add(new Cat)
  val evenMoreAnimals = animals.add(new Dog)

  // METHOD ARGUMENTS ARE IN CONTRAVARIANT POSITION

  // return types
  class PetShop[-T] {
    // def get(isItaPuppy: Boolean): T // METHOD RETURN TYPES ARE IN COVARIANT POSITION
    /*
      val catShop = new PetShop[Animal] {
        def get(isItaPuppy: Boolean): Animal = new Cat
      }

      val dogShop: PetShop[Dog] = catShop
      dogShop.get(true) // EVIL CAT!
     */

    def get[S <: T](isItaPuppy: Boolean, defaultAnimal: S): S = defaultAnimal
  }

  val shop: PetShop[Dog] = new PetShop[Animal]
  // val evilCat = shop.get(true, new Cat)
  class TerraNova extends Dog
  val bigFurry = shop.get(true, new TerraNova)

  /** BIG RULE
    *   - method arguments are in CONTRAVARIANT POSITION
    *   - return types are in COVARIANT position
    */

  /**   1. Invariant, covariant, contravariant
    *
    * Parking[T](things: List[T]) {
    *   - park (vehicle: T)
    *   - impound(vehicles: List[T])
    *   - checkVehicles(conditions: String): List[T]
    * }
    *
    * 2. used someone else's API: IList[I]
    *
    * 3. Parking = monad!
    *   - flatMap
    */
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle

  class IList[T]

  // Invariant
  class IParking[T](vehicles: List[T]) {

    def park(vehicle: T): IParking[T] = ???
    def impound(impounded: List[T]): IParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[S](f: T => IParking[S]): IParking[S] = ???
  }

  //    val car = new Car
  //    val bike = new Bike
  //
  //    val parking = new IParking[Vehicle](List())
  //
  //    parking.park(car)
  //    parking.park(bike)
  //    parking.impound(List(car, bike))
  //    parking.checkVehicles("scala")

  // Covariant

  class CParking[+T](vehicles: List[T]) {

    def park[A >: T](vehicle: A): CParking[T] = ???
    def impound[A >: T](impounded: List[A]): CParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[S](f: T => CParking[S]): CParking[S] = ???
  }

  //        val car = new Car
  //        val bike = new Bike
  //
  //        val parking = new CParking[Vehicle](List())
  //
  //        parking.park(car)
  //        parking.park(bike)
  //        parking.impound(List(car, bike))
  //        parking.checkVehicles("scala")

  // Contravariant
  class CCParking[-T](vehicles: List[T]) {

    def park(vehicle: T): CCParking[T] = ???

    def impound(impounded: List[T]): CCParking[T] = ???

    def checkVehicles[A <: T](conditions: String): List[A] = ???

    def flatMap[R <: T, S](f: R => CCParking[S]): CCParking[S] = ???
  }

  /** RULE OF THUMB:
    *   - Use covariance = COLLECTION OF THINGS
    *   - use contravariance = GROUP OF ACTIONS (TO PERFORM ON YOUR TYPES)
    */

  // exercise 2:
  class CParking2[+T](vehicles: IList[T]) {

    def park[A >: T](vehicle: A): CParking2[T] = ???

    def impound[A >: T](impounded: IList[A]): CParking2[A] = ???

    def checkVehicles[A >: T](conditions: String): IList[A] = ???
  }

  // Contravariant
  class CCParking2[-T](vehicles: IList[T]) {

    def park(vehicle: T): CCParking2[T] = ???

    def impound[A <: T](impounded: IList[A]): CCParking2[A] = ???

    def checkVehicles[A <: T](conditions: String): IList[A] = ???
  }

}
