package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  val superAdder: Int => Int => Int = x => y => x + y
  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))
  println(superAdder(3)(5)) // curried function

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y // curried method

  val add4: Int => Int = curriedAdder(4)
  // lifting = ETA-EXPANSION

  // functions != methods (JVM limitation)
  def inc(x: Int) = x + 1
  List(1,2,3).map(inc) // ETA-Expansion

  // Partial function applications
  val add5 = curriedAdder(5) _ // Int => Int

  // EXERCISE
  val simpleAddFunction = (x:Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y:Int) = x + y

  // add7: Int => Int = y => 7 + y
  // as many differen implementations of add7 using the above
  // be crative!!

  val add7: Int => Int = superAdder(7)
  val add7Curried = curriedAddMethod(7)
  val add7SimpleFunction = (y: Int) => simpleAddFunction(7, y)
  val add7SimpleFunction_v2 = simpleAddFunction(7, _:Int)
  val add7_curried = simpleAddFunction.curried(7)
  val add7ETAExpansion = curriedAddMethod(7) _ // Partially Applied Function -> Scala 2 only
  val add7ETAExpansion_v2 = curriedAddMethod(7)(_) // Partially Applied Function -> Alternative syntax
  val add7SimpleMethod = (y: Int) => simpleAddMethod(7, y)
  val add7_v2Simple = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values
  // y => simpleAddMethod(7, y)

  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _:String, ", how are you?") // x: String => concatenator(hello, x, howareyou)
  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _:String) // (x: String, y: String) => concatenator("hello, ", x, y)

  println(fillInTheBlanks("Daniel", " Scala is awesome!"))

  // EXERCISES
  /**
   * 1. Process a list of numbers and return their string representations with different formats
   *    Use the %4.2f, %8.6f and %14.12f with a curried formatter function
   *    "4.2f".format(Math.PI)
   */

  def stringFormatter(s: String, number: Double) = s.format(number)
  val numbers = List(1.23, 2.33463, 3.445, 4.927618723)

  val simpleFormat = stringFormatter("%4.2f", _: Double)
  val seriousFormat = stringFormatter("%8.6f", _: Double)
  val preciseFormat = stringFormatter("%14.12f", _: Double)

  println(numbers.map(simpleFormat))
  println(numbers.map(seriousFormat))
  println(numbers.map(preciseFormat))

  /**
   * 2. difference between
   *  - Functions vs methods
   *  - parameters: by-name vs 0-lambda
   */

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  /**
   * calling byName and byFunction
   * - int
   * - method
   * - parenMethod
   * - lambda
   * - PAF
   */
  println(byName(42))
  println(byName(method))
  println(byName(parenMethod()))
  // println(byName(() => 42)) // not ok
  println(byName((() => 42)()))
  // println(byName(parenMethod _)) // not ok

  // byFunction(45) // not ok
  // byFunction(method) // not ok!!!!! does not do ETA-expansion!
  byFunction(parenMethod) // compiler does ETA-Expansion
  byFunction(() => 46)
  byFunction(parenMethod _) // unnecessary. Compiler does ETA-expansion automatically
}
