package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChildActors.CreditCard.AttachToAccount

object ChildActors extends App {

  // Actors can create other actors
  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case CreateChild(name) =>
        println(s"[${self.path
        }] Creating child")
        val childRef = context.actorOf(Props[Child], name)
        context.become(withChild(childRef))
    }

    def withChild(childRef: ActorRef): Receive = {
      case TellChild(message) => childRef forward message
    }
  }

  object Parent {
    case class CreateChild(name: String)
    case class TellChild(message: String)
  }

  class Child extends Actor {
    override def receive: Receive = {
      case message => println(s"[${self.path}] I got: $message")
    }
  }

  import Parent._
  val system = ActorSystem("parentChildDemo")
  println(system)
  val parent = system.actorOf(Props[Parent], "parent")
  parent ! CreateChild("child")
  parent ! TellChild("\"hello, kid!\"")

  // Actor hierarchies
  // parent -> child -> grandChild

  /*
    Guardian Actors (top-level)
    - /system = system guardian
    - /user = suer-level guardian
   */

  /**
   * Actor Selection
   */
  val childSelection = system.actorSelection("/user/parent/child")
  childSelection ! "I found you"

  /**
   * Danger!
   *
   * NEVER PASS A MUTABLE ACTOR STATE, OR `THIS` REFERENCE, TO A CHILD ACTOR
   *
   * NEVER IN YOUR LIFE
   */

  object NaiveBankAccount {
    case class Deposit(amount: Int)
    case class Withdraw(amount: Int)
    case object InitializeAccount
  }

  class NaiveBankAccount extends Actor {
    import NaiveBankAccount._
    import CreditCard._
    var funds = 0

    override def receive: Receive = {
      case InitializeAccount =>
        println(s"[${self.path.name}] Creating a credit card")
        val creditCardRef = context.actorOf(Props[CreditCard], "card")
        creditCardRef ! AttachToAccount(this)
      case Deposit(amount) => deposit(amount)
      case Withdraw(amount) => withdraw(amount)
    }

    def deposit(amount: Int) = {
      println(s"[${self.path.name}] depositing $amount on top of $funds")
      funds += amount
    }
    def withdraw(amount: Int) = {
      println(s"[${self.path.name}] withdrawing $amount on top of $funds")
      funds -= amount
    }
  }

  object CreditCard {
    case class AttachToAccount(bankAccount: NaiveBankAccount) // !!
    case object CheckStatus
  }

  class CreditCard extends Actor {
    import CreditCard._
    override def receive: Receive = {
      case AttachToAccount(account) => context.become(attachedTo(account))
    }

    def attachedTo(account: NaiveBankAccount): Receive = {
      case CheckStatus =>
        println(s"${self.path} your message has been processed")
        account.withdraw(1) // this is a real problem
    }
  }

  import NaiveBankAccount._
  import CreditCard._

  val bankAccountRef = system.actorOf(Props[NaiveBankAccount], "account")
  bankAccountRef ! InitializeAccount
  bankAccountRef ! Deposit(100)

  Thread.sleep(500)
  val creditCardSelection = system.actorSelection("/user/account/card")
  creditCardSelection ! CheckStatus


}
