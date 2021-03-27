package playground

import java.util.Calendar

import akka.actor.{Actor, ActorSystem, Props, Timers}

import scala.annotation.tailrec
import scala.util.Random
import scala.concurrent.duration._
import scala.language.postfixOps


object LeibnizScheduler extends App {

  case object Leibniz {

    val random = new Random()

    def apply(n: Int = random.nextInt(500)): Double = {
      @tailrec
      def calculus(x: Double, acc: Double): Double = {
        if (x >= n) acc
        else {
          val res: Double = 1 / (2 * x + 1)
          if (x % 2 == 0) calculus(x + 1, acc + res)
          else calculus(x + 1, acc - res)
        }
      }
      calculus(0.0, 0.0) * 4
    }
  }

  class PiTimer extends Actor with Timers {
    import PiTimer._

    override def receive: Receive = {
      case "start" => timers.startTimerAtFixedRate(TimerKey, Leibniz, 5 seconds)
      case Leibniz => println(Calendar.getInstance().getTime + " - " + Leibniz())
    }
  }

  object PiTimer {
    case object TimerKey
  }

  val system = ActorSystem("LeibnizDemo")
  val piActor = system.actorOf(Props[PiTimer], "piActor")
  piActor ! "start"

}
