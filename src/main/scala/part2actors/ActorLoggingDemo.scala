package part2actors


import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging

object ActorLoggingDemo extends App {

  class SimpleActorExplicitLogger extends Actor {
    // #1 - exp
    val logger = Logging(context.system, this)

    override def receive: Receive = {
      case message => logger.info(message.toString)// LOG IT
        /*
          1 - DEBUG - most verbose
          2 - INFO - most used
          3 - WARNING/WARN - deadletters
          4 - ERROR -
         */
    }
  }
  val system = ActorSystem("LoggingDemo")
  val actor = system.actorOf(Props[SimpleActorExplicitLogger], "actorLogger")
  actor ! "Logging a simple message"

  // #2 - ActorLogging
  class ActorWithLogging extends Actor with ActorLogging {
    override def receive: Receive = {
      case (a, b) => log.info("Two things: {} and {}", a, b)
      case message => log.info(message.toString)
    }
  }

  val simplerActor = system.actorOf(Props[ActorWithLogging], "simplerActor")
  simplerActor ! "Another logging message"
  simplerActor ! (2, 3)
}
