package lectures.part4implicits

import scala.annotation.tailrec

object ExtensionMethods extends App {

  case class Person(name: String) {
    def greet(): String =  s"Hi, I'm name $name, how can I help?"
  }

  extension(string: String) {
    def greetAsPerson(): String = Person(string).greet()
  }

  val danielsGreeting = "Daniel".greetAsPerson()

  // extension methods <=> implicit classes

  object Scala2ExtensionMethods {
    implicit class RichInt(val value: Int) {
      def isEven: Boolean = value % 2 == 0

      def sqrt: Double = Math.sqrt(value)

      def times(f: () => Unit): Unit = {
        @tailrec
        def timesAux(n: Int): Unit =
          if (n <= 0) ()
          else {
            f()
            timesAux(n - 1)
          }

        timesAux(value)
      }
    }
  }

  val is3Even = 3.isEven

  extension (value: Int) {
    // define all methods
    def isEven: Boolean = value % 2 == 0

    def sqrt: Double = Math.sqrt(value)

    def times(f: () => Unit): Unit = {
      @tailrec
      def timesAux(n: Int): Unit =
        if (n <= 0) ()
        else {
          f()
          timesAux(n - 1)
        }

      timesAux(value)
    }
  }

  // generic extensions
  extension [A](list: List[A]) {
    def ends: (A, A) = (list.head, list.last)
    def extremes(using ordering: Ordering[A]): (A, A) = list.sorted.ends // <-- can call an extension method here
  }
}
