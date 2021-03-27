package playground

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Studies1 extends App {

  val system = ActorSystem("actorSystem")

  class SimpleActor extends Actor {
    import SimpleActor._

    override def receive: Receive = {
      case message: String => println(s"[${self.path.name}] I have received: $message")
      case number: Int => println(s"[${self.path.name}] I have received a number: $number")
      case SpecialMessage(contents, ref) => ref ! contents
      case VerySpecialMessage(ref) => List(self.path.name, sender(), ref.path.name).foreach(println)
    }
  }

  object SimpleActor {
    case class SpecialMessage(contents: String, ref: ActorRef)
    case class VerySpecialMessage(ref: ActorRef)
  }

  import SimpleActor._
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")
  simpleActor ! "\"hello, there\""
  simpleActor ! 42

//  simpleActor ! SpecialMessage("This is a special message")

  val anotherActor = system.actorOf(Props[SimpleActor], "anotherActor")
  simpleActor ! SpecialMessage("Hello from a simple actor", anotherActor)
  simpleActor ! VerySpecialMessage(anotherActor)

  Thread.sleep(1000)
  println("- - - - - - - - - - - - - - - - - ")

  object Parent {
    case class Initialize(number: Int)
    case class GiveTask(text: String)
    case class TaskReply(res: Int)
  }
  class Parent extends Actor {
    import Parent._

    override def receive: Receive = {
      case Initialize(n) =>
        for (i <- 1 to n) yield {
        println(s"[${self.path.name}] creating child")
        context.actorOf(Props[Child], s"child_$i")
        }
    }
  }

  object Child {
    case class GiveReply(n: Int)
  }

  class Child extends Actor {
    import Parent._

    override def receive: Receive = {
      case GiveTask(text) =>
        println(s"[${self.path.name}] '$text' is ${text.split(" ").length} words long")
    }
  }

  import Parent._
  val parent = system.actorOf(Props[Parent], "father")
  parent ! Initialize(3)
  Thread.sleep(500)

  val children = for {
    i <- 1 to 3
  } yield system.actorSelection(s"/user/father/child_$i")
  val list = List(
    "Learning Akka is really cool",
    "It's difficult, but I enjoy it",
    "I'm stubborn and don't give up easily"
  )

  (0 to 2).foreach(i => children(i) ! GiveTask(list(i)))
}
