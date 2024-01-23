package exercises

import lectures.part4implicits.TypeClasses.User

object EqualityPlayground  extends App {

  /**
   * Equality
   */
  trait Equal[T] {
    def apply(value1: T, value2: T): Boolean
  }

  object Equal {
    def apply[T](value1: T, value2: T)(implicit equal: Equal[T]): Boolean = {
      equal.apply(value1, value2)
    }
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(user1: User, user2: User): Boolean = user1.name == user2.name
  }

  object FullEquality extends Equal[User] {
    override def apply(user1: User, user2: User): Boolean = (user1.name == user2.name) && (user1.email == user2.email)
  }

  val john = User("John", 32, "john@rockthejvm")
  val anotherJohn = User("John", 45, "anotherJohn@rtjvm.com")
  println(Equal(anotherJohn, john))
  // AD-HOC polymorphism

  /**
   * - type class itself -> HTMLSerializer[T] { .. }
   * - type class instances (some of which are implicit) -> UserSerializer, IntSerializer
   * - conversion with implicit classes -> HTMLEnrichment
   */

  /*
    Exercise: improve the Equal TC with an implicit conversion class
    ===(anotherValue: T)
    !==(anotherValue: T)
   */

  implicit class TypeSafeEqual[T](value: T) {
    def ===(other: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(value, other)
    def !==(other: T)(implicit equalizer: Equal[T]): Boolean = !equalizer.apply(value, other)
  }

  println(john === anotherJohn)

  /**
   * john.===(anotherJohn)
   * new TypeSafeEqual[User](john).===(anotherJohn)
   * new TypeSafeEqual[User](john).===(anotherJohn)(NameEquality)
   */

  /*
    TYPE SAFE
   */
  // println(john == 43) -> Does not compile in Scala 3
  // println(john === 43) // TYPE SAFE
}
