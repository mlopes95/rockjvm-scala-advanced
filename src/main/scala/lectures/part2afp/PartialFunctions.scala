package lectures.part2afp

import jdk.nashorn.internal.ir.FunctionNode

import scala.io.Source.stdin

object PartialFunctions extends App {

  val aFunction = (x: Int) => x + 1 // Function1[Int, Int] === Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new FuntionNotApplicableException

  class FuntionNotApplicableException extends RuntimeException

  val aNicerFussyFunction = (x: Int) =>
    x match {
      case 1 => 42
      case 2 => 56
      case 3 => 999
    }
  // {1,2,5} => Int

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 3 => 999
  } // partial function value

  println(aPartialFunction(2))
  // println(aPartialFunction(57273))

  // PF utilities
  println(aPartialFunction.isDefinedAt(67))

  // lift
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(98))

  val pfChain = aPartialFunction.orElse[Int, Int] { case 45 =>
    67
  }

  println(pfChain(2))
  println(pfChain(45))

  // PF extend normal functions

  val aTotalFunction: Int => Int = { case 1 =>
    99
  }

  // HOFs accept partial functions as well

  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }

  println(aMappedList)

  /*
    PF can only have ONE parameter type
   */

  /** Exercises 1 - construct a PF instance yourself (anonymous class) 2 - dumb chatbot as a PF
    */

  val myPartialFunction: PartialFunction[Int, Int] = new PartialFunction[Int, Int] {

    override def apply(v1: Int): Int =
      v1 match {
        case 1 => 42
        case 2 => 56
        case 5 => 999
      }

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5

  }

  val chatbot: PartialFunction[String, String] = {
    case "hello" => s"Hello! How can I help you?"
    case "tell me a joke" => s"I do not have the facilities for that big man!"
    case "goodbye" => s"Bye, have a great day!"
  }

  stdin.getLines().map(chatbot).foreach(println)
}
