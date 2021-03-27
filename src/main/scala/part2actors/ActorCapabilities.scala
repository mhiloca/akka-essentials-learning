package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ActorCapabilities.Person.LiveTheLife

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    context.self
    override def receive: Receive = {
      case "Hi!" => sender() ! "Hello, there!"
      case message: String => println(s"[${self}] I have received a message $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor]I have receive something SPECIAL: $contents")
      case SendMessageToYourself(content) =>
        self ! content
      case SayHiTo(ref) => ref ! "Hi!" // alice is being passed as the sender
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s") // I keep the original sender of the WPM
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  //1- messages can be of any type
  // a) messages must be IMMUTABLE
  // b) messages must be SERIALIZABLE
  // in practice use case classes and case objects
  simpleActor ! 42

  case class SpecialMessage(contents: String)
  simpleActor ! SpecialMessage("some special content")

  // 2- actors have information about their context and about themselves
  // context.self === this
  case class SendMessageToYourself(content: String)
  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it!")

  // 3- actors can reply to messages
  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  case class SayHiTo(ref: ActorRef)

  alice ! SayHiTo(bob)

  // 4- dead letters
//  alice ! "Hi!"

  // 5- forwarding messages
  // forwarding = sending a message with the ORIGINAL sender

  case class WirelessPhoneMessage(content: String, ref: ActorRef)

  alice ! WirelessPhoneMessage("Hi!", bob)

  /*
    Exercises
    1. a counterActor (hold an internal variable)
      - increment
      - decrement
      - print

    2. a bank account as an actor
      it withhold an internal variable to hold the funds

      receives messages
      - deposit
      - withdraw and amount
      - statement
      replies with
      - Success
      - Failure

      interact with some other kind of actor - that will send a deposit or a withdraw to another actor

   */

  class Counter extends Actor {
    import Counter._
    var state = 0

    override def receive: Receive = {
      case Increment => state += 1
      case Decrement => state -=1
      case Print => println(s"[counter]Current count: $state")
    }
  }

  // DOMAIN of the actor
  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  import Counter._
  val counter = system.actorOf(Props[Counter], "counter")

  (1 to 5).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print

  class BankAccount extends Actor {
    import BankAccount._
    var funds = 0
    override def receive: Receive = {
      case Deposit(money) => {
        if (money < 0) sender() ! TransactionFailure("Cannot resolve deposit with negative value")
        else {
          funds += money
          sender() ! TransactionSuccess("Deposit succeeded")
        }
      }
      case Withdraw(money) =>  {
        if (funds == 0 || money > funds) sender() ! TransactionFailure("Not enough funds for withdraw")
        else {
          funds -= money
          sender() ! TransactionSuccess("Withdraw succeeded")
        }
      }
      case Statement => sender() ! s"Current balance: $funds"
    }
  }

  object BankAccount {

    case class Deposit(money: Int)
    case class Withdraw(money: Int)
    case object Statement

    case class TransactionSuccess(message: String)
    case class TransactionFailure(message: String)

  }

  class Person extends Actor {
    import BankAccount._
    import Person._
    override def receive: Receive = {
      case LiveTheLife(account) =>
        account ! Deposit(10000)
        account ! Withdraw(50000)
        account ! Withdraw(5000)
        account ! Statement
      case message => println(message.toString)
    }
  }

  object Person {
    case class LiveTheLife(account: ActorRef)
  }

  val me = system.actorOf(Props[Person], "me")
  val account = system.actorOf(Props[BankAccount], "account")

  me ! LiveTheLife(account)

}
