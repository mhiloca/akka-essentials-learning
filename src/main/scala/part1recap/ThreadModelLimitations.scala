package part1recap

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ThreadModelLimitations extends App {

  /*
    Daniel's rant
    #1: OOP encapsulation is only valid in the SINGLE THREADED MODEL
   */

  class BankAccount(private var amount: Int) {
    override def toString: String = "" + amount

    def withdraw(money: Int) = this.synchronized {
      this.amount -= money
    }

    def deposit(money: Int) = this.synchronized {
      this.amount += money
    }

    def getAmount = amount
  }

  //
  //  val account = new BankAccount(2000)
  //  for (_ <- 1 to 1000) {
  //    new Thread(() => account.withdraw(1)).start()
  //  }
  //
  //  for (_ <- 1 to 1000) {
  //    new Thread(() => account.deposit(1)).start()
  //  }

  //  println(account.getAmount)

  // OOP encapsulation is broken in a multithreaded ev
  // synchronization" Locks to the rescue
  // deadlocks, livelocks

  // #2 - delegating to a Thread is a PAIN

  // executor service
  // you have a running thread and you want to pass a runnable to that thread.

  var task: Runnable = null
  val runningThread: Thread = new Thread(() => {
    while (true) {
      while (task == null) {
        runningThread.synchronized {
          println("[background] waiting for a task...")
          runningThread.wait()
        }
      }
      task.synchronized {
        println("[background] I have a task")
        task.run()
        task = null
      }
    }
  })

  def delegateToBackroundThread(r: Runnable) = {
    if (task == null) task = r
    runningThread.synchronized {
      runningThread.notify()
    }
  }

  runningThread.start()
  Thread.sleep(500)
  delegateToBackroundThread(() => println(42))
  Thread.sleep(1000)
  delegateToBackroundThread(() => println("this should run in the background"))

  // #3: tracing and dealing with errors in a multithreaded env is a PAIN
}
