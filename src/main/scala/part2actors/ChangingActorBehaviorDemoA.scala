package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}


object ChangingActorBehaviorDemoA extends App {

  val system = ActorSystem("changingActorBehaviorDemoA")

  object Kid {
    case object KidSaysYes
    case object KidSaysNo
  }

  class Kid extends Actor {
    import Kid._
    import Mom._
    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(CHOCOLATE) =>
      case Food(VEGETABLES) => context.become(sadReceive, discardOld=false) // context.become(sadReceive, true) by default
      case Ask(_) => sender() ! KidSaysYes
    }
    def sadReceive: Receive = {
      case Food(CHOCOLATE) => context.unbecome()
      case Food(VEGETABLES) => context.become(sadReceive, discardOld=false)
      case Ask(_) => sender() ! KidSaysNo
    }
  }

  object Mom {
    val VEGETABLES = "veggies"
    val CHOCOLATE = "chocolate"

    case class Food(food: String)
    case class Ask(msg: String)
    case class MomStarts(ref: ActorRef)
  }

  class Mom extends Actor {
    import Mom._
    import Kid._
    override def receive: Receive = {
      case MomStarts(kid) =>
        kid ! Food(VEGETABLES)
        kid ! Food(VEGETABLES)
        kid ! Food(CHOCOLATE)
        kid ! Food(CHOCOLATE)
        kid ! Ask("Do you want to play?")
      case KidSaysYes => println("My kid is happy!")
      case KidSaysNo => println("My kid is sad, but healthy!")
    }
  }

  val mom = system.actorOf(Props[Mom], "mom")
  val kid = system.actorOf(Props[Kid], "kid")

  import Mom._
  mom ! MomStarts(kid)

  /*
  -> context.become
  Akka supports hotswapping the Actor's message loop at runtime invoking context.become
  The hotswapped code is kept in a Stack which ca be pushed and popped

  default: context.become(<method_name>, discardOld=true)
    this means, completely discard the old message handler and fully replace for the one specified
  context.become(<method_name>, discardOld=false)
    it stacks the specified message handler to the Stack
    ex:
      Food(veg) -> stack.push(sadReceive)
      Food(choc) -> stack.push(happyReceive)

    Stack:
    1. happyReceive
    2. sadReceive
    3. happyReceive


   -> context.unbecome
   It pops the message handler from the behavior stack
   In this case, care must be taken to ensure that the number of "pop" operations matches the number of "push" ones
   in the long run, otherwise this amounts to a memory leak - which is why this is not the default

   Food(veg)
   Food(veg)
   Food(choc)
   Food(choc)

   Stack:
   1. happyReceive

   */

  /**
   * Exercises
   * 1 - recreate the Counter Actor with context.become and NO MUTABLE STATE
   */
  class Counter extends Actor {
    import Counter._

    override def receive: Receive = countReceive(0)

    def countReceive(currentCount: Int): Receive = {
      case Increment => context.become(countReceive(currentCount + 1))
      case Decrement => context.become(countReceive(currentCount - 1))
      case Print => println(s"Current count: $currentCount")
    }
  }

  object Counter {
    case object Increment
    case object Decrement
    case object Print
  }

  import Counter._
  val counter = system.actorOf(Props[Counter], "counter")

  (1 to 10).foreach(_ => counter ! Increment)
  (1 to 3).foreach(_ => counter ! Decrement)
  counter ! Print


  /**
   * Exercise 2 - a simplified voting system
   */

  object Citizen {
    case class Vote(candidate: String)
    case class VoteStatusReply(candidate: Option[String])
  }

  class Citizen extends Actor {
    import Citizen._
    import VoteAggregator._
    override def receive: Receive = {
      case Vote(c) =>
        context.become(voteReceive(c))
    }

    def voteReceive(candidate: String): Receive = {
      case VoteStatusRequest => sender() ! VoteStatusReply(Some(candidate))
    }
  }

  class VoteAggregator extends Actor {
    import VoteAggregator._
    import Citizen._

    override def receive: Receive = {
      case AggregateVotes(citizens) =>
        citizens.foreach(citizenRef => citizenRef ! VoteStatusRequest)
        context.become(aggregateReceive(Map()))
    }

    def aggregateReceive(status: Map[String, Int]): Receive = {
      case VoteStatusReply(candidate) =>
        val candidateVotes = status.getOrElse(candidate.get, 0)
        val currentStatus = status + (candidate.get -> (candidateVotes + 1))
        context.become(aggregateReceive(currentStatus))
        println(s"[${self.path.name}] poll stats: $currentStatus")
    }
  }

  object VoteAggregator {
    case object VoteStatusRequest
    case class AggregateVotes(citizens: Set[ActorRef])
  }

  val alice = system.actorOf(Props[Citizen], "alice")
  val bob = system.actorOf(Props[Citizen], "bob")
  val daniel = system.actorOf(Props[Citizen], "daniel")
  val charlie = system.actorOf(Props[Citizen], "charlie")

  import Citizen._
  import VoteAggregator._
  alice ! Vote("Martin")
  bob ! Vote("Jonas")
  daniel ! Vote("Roland")
  charlie ! Vote("Roland")

  val voteAggregator = system.actorOf(Props[VoteAggregator], "aggregator")
  voteAggregator ! AggregateVotes(Set(alice, bob, daniel, charlie))

  /*
    Print the status of the votes: Map(candidate -> numberOfVotes)

    Martin -> 1
    Jonas -> 1
    Roland -> 2
   */

}
