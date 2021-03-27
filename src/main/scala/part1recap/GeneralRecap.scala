package part1recap

import scala.annotation.tailrec
import scala.reflect.runtime.universe.Try

object GeneralRecap {

  val aCondition: Boolean = false

  var aVariable = 42
  aVariable += 1


  // expressions
  val aConditionVal: Int = if (aCondition) 42 else 65

  // code block
  val aCodeBlock: Int = {
    if (aCondition) 74
    56
  }

  // types
  // Unit
  val theUnit: Unit = println("Hello, Scala!") // side effects

  // functions
  def aFunction(x: Int): Int = x + 1

  // recursion - tailrec

  @tailrec
  def factorial(n: Int, acc: Int = 1): Int ={
    if (n <= 0) acc
    else factorial(n - 1, acc * n)
  }

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    override def eat(a: Animal): Unit = println("crunch, crunch")
  }

  // method notations
  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar")
  }

  aCarnivore.eat(aDog)

  // generics
  abstract class MyList[+A]

  // companion objects
  object MyList

  // case classes
  case class Person(name: String, age: Int) // a LOT in this course!

  // Exceptions
  val aPotentialFailures = try {
    throw new RuntimeException("I'm innocent, I swear")
  } catch {
    case e: Exception => "I caught an exception!"
  } finally {
    // side effects
    println("some logs")
  }

  // Functional Programming
  val incrementer = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  val incremented = incrementer(42) // 43
  // incrementer.apply(42)

  val anonymousIncrementer = (x: Int) => x + 1
  // Int => Int === Function1[Int, Int]

  // FP is all about working with functions as first-class
  List(1, 2, 3).map(incrementer)
  // map = HOF

  // for-comprehensions
  val pairs = for {
    num <- (1 to 4).toList
    char <- List('a', 'b', 'c', 'd')
  } yield num + "-" + char

  // Seq, Arrays, List, Vector, Map, Tuples, Sets

  //"collections"
  // Options, Try
  val anOption = Some(2)
  val aTry = Try {
    throw new RuntimeException
  }

  // pattern matching
  val unknow = 2
  val order = unknow match  {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val bob = Person("Bob", 22)

  val greeting = bob match {
    case Person(n, _) => s"Hi, my name is $n"
    case _ => "I don't know my name"
  }

}
