package playground

import akka.actor.ActorSystem

object Playground {

  val actorSystem = ActorSystem("HelloAkka")
  println(actorSystem.name)

}
