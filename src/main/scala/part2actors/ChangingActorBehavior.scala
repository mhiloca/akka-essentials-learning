package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChangingActorBehavior extends App {

  val system = ActorSystem("changingActorBehaviorDemo")

  object FussyKid {
    case object KidAccept
    case object KidReject

    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FussyKid extends Actor {
    import FussyKid._
    import Mom._

    // internal state of the kid
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }

  class StatelessFussyKid extends Actor {
    import FussyKid._
    import Mom._
    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive) // change my receive handler to sadReceive
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }
    def sadReceive: Receive = {
      case Food(CHOCOLATE) => context.become(happyReceive) // change my receive handler to happyReceive
      case Food(VEGETABLE) =>
      case Ask(_) => sender() ! KidReject
    }
  }

  object Mom {
    case class Food(food: String)
    case class Ask(message: String) // do you want to play
    case class MomOffers(kid: ActorRef)

    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Mom extends Actor {
    import FussyKid._
    import Mom._

    override def receive: Receive = {
      case MomOffers(kid) =>
        kid ! Food(VEGETABLE)
        kid ! Ask("Would you like to play?")
      case KidAccept => println("Yay, my kid is happy!")
      case KidReject => println("Not so happy now :( ")
    }
  }

  val kid = system.actorOf(Props[FussyKid], "kid")
  val mom = system.actorOf(Props[Mom], "mom")
  val statelessFussyKid = system.actorOf(Props[StatelessFussyKid], "statelessFussyKid")

  import Mom._
  mom ! MomOffers(kid)
  mom ! MomOffers(statelessFussyKid)

  /*
    mom receives MomOffers
      kid receives Food(veg) -> kid will change the handler to sadReceive
      kid receives Ask(msg) -> kid replies with the sadReceive Handler =>
    mom receives KidReject
   */


}
