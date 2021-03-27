package part3testing

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.testkit.{CallingThreadDispatcher, EventFilter, TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.Duration

class SynchronousTestingSpec extends AnyWordSpecLike
  with BeforeAndAfterAll
{
  import SynchronousTestingSpec._

  implicit val system = ActorSystem("SynchronousTestingSpec")

  override def afterAll(): Unit = {
    system.terminate()
  }

  "A counter" should {
    "synchronously increase its counter" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter ! Inc // counter has ALREADY received the message
    }

    "synchronously increase its counter at the call of the receive function" in {
      val counter = TestActorRef[Counter](Props[Counter])
      counter.receive(Inc)
//      assert(counter.underlyingActor.count == 1)
    }

    "work on the calling thread dispatcher" in  {
      val counter = system.actorOf(Props[Counter].withDispatcher(CallingThreadDispatcher.Id))
      val probe = TestProbe()
      for (_ <- 1 to 10) {counter ! Inc}
      probe.send(counter, Read)
      probe.expectMsg(Duration.Zero, 10) // probe has already received the message 0
    }
  }
}

object SynchronousTestingSpec {
  case object Inc
  case object Read

  class Counter extends Actor with ActorLogging {
    override def receive: Receive = countReceive(0)

    def countReceive(count: Int): Receive = {
      case Inc => context.become(countReceive(count + 1))
      case Read => sender() ! count
    }
  }
}
