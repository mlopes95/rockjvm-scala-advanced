package exercises

import scala.annotation.tailrec

abstract class MyStream[+A] {

  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend operator

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  def take(n: Int): MyStream[A] // takes the first n elements out of this stream

  def takeAsList(n: Int): List[A] = take(n).toList()

  /** [1 2 3].toList([]) = [2 3].toList([1]) [3].toList([2 1]) [].toList([3 2 1]) \= [1 2 3]
    */
  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)

}

case object EmptyStream extends MyStream[Nothing] {

  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException()

  def tail: MyStream[Nothing] = throw new NoSuchElementException()

  def #::[B >: Nothing](element: B): MyStream[B] = new NonEmptyStream[B](element, this)

  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  def take(n: Int): MyStream[Nothing] = this

}

class NonEmptyStream[+A](h: A, t1: => MyStream[A]) extends MyStream[A] {

  def isEmpty: Boolean = false

  override val head: A = h
  override lazy val tail: MyStream[A] = t1 // call by need

  /** val s = new NonEmptyStream(1, EmptyStream) val prepend = 1 #:: s = new Cons(1, s)
    */
  def #::[B >: A](element: B): MyStream[B] = new NonEmptyStream[B](element, this)

  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] =
    new NonEmptyStream[B](head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  /** s = new NonEmptyStream(1, ?) mapped = s.map(_ + 1) = new NonEmptyStream(2, s.tail.map(_ + 1))
    * .... mapped.tail
    */
  def map[B](f: A => B): MyStream[B] = new NonEmptyStream[B](f(head), tail.map(f))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new NonEmptyStream(head, tail.filter(predicate))
    else tail.filter(predicate) // preserves lazy eval

  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new NonEmptyStream(head, EmptyStream)
    else new NonEmptyStream(head, tail.take(n - 1))

}

object MyStream {

  def from[A](start: A)(generator: A => A): MyStream[A] =
    new NonEmptyStream(start, MyStream.from(generator(start))(generator))

}

object StreamsPlayground extends App {

  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals // naturals.#::(0)
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  // map, flatMap
  println(startFrom0.map(_ * 2).take(100).toList())

  println(
    startFrom0
      .flatMap(x => new NonEmptyStream(x, new NonEmptyStream(x + 1, EmptyStream)))
      .take(10)
      .toList()
  )

  println(startFrom0.filter(_ < 10).take(10).toList())

  /**
   * Exercises:
   * 1. Stream of fibonacci numbers
   * 2. Implement the stream of prime numbers with Eratosthenes' sieve
   * [ 2 3 4 ...]
   * filter out all numbers divisible by 2
   * [ 2 3 5 7 9 11 ... ]
   * filter out all numbers divisible by 3
   * [ 2 3 5 7 11 13 17 ... ]
   * filter out all numbers divisible by 5
   */

  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] = {
    new NonEmptyStream[BigInt](first, fibonacci(second, first + second))
  }

  println(fibonacci(1, 1).take(100).toList())

  // eratosthenes sieve
  def erastosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else
      new NonEmptyStream[Int](
        numbers.head,
        erastosthenes(numbers.tail.filter(n => n % numbers.head != 0))
      )

  println(erastosthenes(MyStream.from(2)(_ + 1)).take(100).toList())

}
