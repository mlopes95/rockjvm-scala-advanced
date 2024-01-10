package lectures.part1as

import scala.EmptyTuple

object AdvancedPatternMatching extends App {

  val numbers = List(1)
  val description = numbers match {
    case head :: Nil => println(s"The only element is $head.")
    case _ =>
  }

  /*
    - constants
    - wildcards
    - case classes
    - tuples
    - some special magic like above
   */

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      if (person.age < 21) None
      else Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", age = 25)
  val greeting = bob match
    case Person(n, a) => s"Hi, my name is $n and I am $a yo."

  println(greeting)

  val legalStatus = bob.age match
    case Person(status) => s"My legal status is $status"

  println(legalStatus)

  /*
    Exercise.
   */

  object NumbersPattern {
    def unapply(n: Int): Option[String] =
      if(n < 10) Some("single digit")
      else if(n % 2 == 0) Some("an even number")
      else Some("no property")
  }

  object Even {
    def unapply(n: Int): Boolean = n % 2 == 0
  }

  object SingleDigit {
    def unapply(n: Int): Boolean = n  > -10 && n < 10
  }

  val n: Int = 8
  val mathProperty = n match
    case SingleDigit() => "single digit"
    case Even() => "an even number"
    case _ => "no property"

  println(mathProperty)

  //infix patterns
  case class Or[A, B](a: A, b: B)
  val either = Or(2, "two")
  val humanDescription = either match
    case number Or string => s"$number is written as $string"
  println(humanDescription)

  // decomposing sequences
  val vararg = numbers match
    case List(1, _*) => "starting with 1"

  abstract class MyList[+A] {
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]

  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyList {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if(list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match
    case MyList(1,2,_*) => "starting with 1, 2"
    case _ => "Something else"

  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }

  println(bob match
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  )
  }
}
