package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ChildActorsExercise extends App {

  // Distributed Word Count
  object WordCounterMaster {
    case class Initialize(nChildren: Int)
    case class WordCountTask(text: String)
    case class WordCountReply(text: String, count: Int)
    case class TextGive(texts: String)
  }

  class WordCounterMaster extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case Initialize(nChildren) =>
        val childrenRefs = for (i <- 1 to nChildren) yield {
          context.actorOf(Props[WordCounterWorker], s"worker_$i")
        }
        context.become(taskReceive(childrenRefs, 0))
    }

    def taskReceive(childrenRefs: Seq[ActorRef], taskIndex: Int): Receive = {
      case TextGive(text) => {
        val childIndex = taskIndex % childrenRefs.size
        childrenRefs(childIndex) forward WordCountTask(text)
        context.become(taskReceive(childrenRefs, taskIndex + 1))
      }
    }
  }

  class WordCounterWorker extends Actor {
    import WordCounterMaster._
    override def receive: Receive = {
      case WordCountTask(text) =>
        val wordCount = text.split(" ").length
        sender() ! WordCountReply(text, wordCount)
    }
  }

  class Requester extends Actor {
    import WordCounterMaster._

    val texts: Seq[String] = Seq(
      "I'm learning Akka Actors Model",
      "I like Scala language very much",
      "It's a difficult language, but I enjoy it",
      "I'm stubborn and persistent",
      "Sometimes dumb as well",
      "And incredibly tired today!"
    )

    override def receive: Receive = {
      case "start" =>
        val master = context.actorOf(Props[WordCounterMaster], "master")
        master ! Initialize(3)
        texts.foreach(text => master ! TextGive(text))
      case WordCountReply(text, wordCount) =>
        println(s"[${sender().path.name}] '$text': $wordCount words long")
    }
  }

  val system = ActorSystem("childActorsExercise")
  val requester = system.actorOf(Props[Requester], "requester")
  requester ! "start"

  /*
    create wordCounterMaster
    send Initialize(10) to wordCounterMaster
    send "Akka is awesome" to wordCounterMaster
      wcm will send a WordCountTask("...")  to one of its children
        child replies with a WordCountReply(3) to the master
      master replies with 3 to the sender.

     requester -> wcm -> wcw
              r <- wcm <-

     // round tobin logic
     // 1, 2, 3, 4, 5, and 7 tasks
     // 1, 2, 3, 4, 5, 1, 2
    */
  
}
