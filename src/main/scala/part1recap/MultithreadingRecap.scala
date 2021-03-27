package part1recap

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object MultithreadingRecap extends App {

//  val aThread = new Thread(() => println("I'm running in parallel"))
//  aThread.start()
//  aThread.join()

  val threadHello = new Thread(() => (1 to 1000).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 1000).foreach(_ => println("goodbye")))

  // different runs produce different results!
  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount

    def withdraw(money: Int) = this.amount -= money

    def safeWithdraw(money: Int) = this.synchronized {
      this.amount -= money
    }
  }

  /*
  Ba(10000)
  T1 -> withdraw 1000
  T2 -> withdraw 2000

  T1 -> this.amount = this.amount - ... // PREEMPTED by the OS
  T2 -> this.mount = this.amount - 2000 = 8000
  T1 -> -1000 = 9000

  => result = 9000
  this.amount = this.amount - 1000 is NOT ATOMIC
   */

  // inter-thread communication on the JVM
  // wait - notify mechanism

  // Scala Futures
  val future = Future {
    42
  }

//  future.onComplete {
//    case Success(42) => println("I found the meaning of life")
//    case Failure(_) => println("Something happened to the meaning of life")
//  }

  val aProcessedFuture = future.map(_ + 1) // Future with 43
  val aFlatFuture = future.flatMap {
    value => Future(value + 42)
  }

  aFlatFuture.onComplete {
    case Success(value) => println(s"This is the future $value")
    case Failure(e) => println("No future for you and me")
  }

  val filteredFuture = future.filter(_ % 2 == 0)

  val aNonsenseFuture = for {
    meaningOfLife <- future
    filteredMeaning <- filteredFuture
  } yield meaningOfLife + filteredMeaning

  aNonsenseFuture.onComplete {
    case Success(value) => println(s"meaning of Life $value")
    case Failure(e) => println(e)
  }

  Thread.sleep(5000)

  // andThen, recover/ recoverWith

  // Promises

}
