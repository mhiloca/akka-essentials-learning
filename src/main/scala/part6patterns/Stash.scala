package part6patterns

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Stash}

object Stash extends App {

  /*
    ResourceActor
      - open => it can receive read/write requests to the resource
      - otherwise it will postpone all read/write requests until the state is open

      ResourceActor is closed
        - Open => switch to the open state
        - Read, Write messages are POSTPONED

      ResourceActor is open
        - Read, Write are handled
        - Close => switch to the closed state

      [Open, Read, Read, Write]
      - switch to the open state
      - read the data
      - read the data
      - write the data

      [Read, Open, Write]

      - stash Read
        Stash: [Read]
      - open => switch to the opens state
        Mailbox: [Read. Write]
      - read, write are handled
   */

  case object Open
  case object Close
  case object Read
  case class Write(data: String)

  // step1 - mix-in the Stash trait
  class ResourceActor extends Actor with ActorLogging with Stash {
    private var innerData: String = ""

    override def receive: Receive = closed

    def closed: Receive = {
      case Open =>
        log.info("Opening resource")
        // step 3 - unstasshAll when you switch the message handler
        unstashAll()
        context.become(opened)
      case message =>
        log.info(s"Stashing: $message - because I can't handle it in the closed state")
        // step 2 - stash away what you can't handle
        stash()
    }

    def opened: Receive = {
      case Read =>
        // do some actual computation
        log.info(s"I have read $innerData")
      case Write(data) =>
        log.info(s"I'm writing $data")
        innerData = data
      case Close =>
        log.info("Closing resource")
        context.become(closed)
    }
  }

  val system = ActorSystem("StashDemo")
  val resourceActor = system.actorOf(Props[ResourceActor])

  resourceActor ! Read // stashed
  resourceActor ! Open // switch to the open state
  resourceActor ! Write("I love stash") //
  resourceActor ! Close
  resourceActor ! Read



}
