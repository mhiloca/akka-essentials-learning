package part5infra

import akka.actor.{Actor, ActorLogging, ActorSystem, Props, Terminated}
import akka.routing.{ActorRefRoutee, Broadcast, FromConfig, RoundRobinGroup, RoundRobinPool, RoundRobinRoutingLogic, Router}
import com.typesafe.config.ConfigFactory

object Routers extends App {

  /**
   * #1 - manual router
   */

  class Master extends Actor {
    // step 1 - create routees
    // 5 actors routees based off Slave actors
    private val workers = for (i <- 1 to 5) yield {
      val worker = context.actorOf(Props[Worker], s"worker_$i")
      context.watch(worker)
      ActorRefRoutee(worker)
    }

    // step 2 - define de router
    private var router = Router(RoundRobinRoutingLogic(), workers)

    override def receive: Receive = {
      // step 4 - handle the termination of the routees
      case Terminated(ref) =>
        router.removeRoutee(ref)
        val newWorker = context.actorOf(Props[Worker])
        context.watch(newWorker)
        router = router.addRoutee(newWorker)

      // step 3 - route the message
      case message =>
        router.route(message, sender())

    }
  }


  class Worker extends Actor with ActorLogging {
    override def receive: Receive = {
      case message => log.info(s"[${self.path.name}] - ${message.toString}")
    }
  }

  val system = ActorSystem("RouterDemo", ConfigFactory.load().getConfig("routersDemo"))
  val master = system.actorOf(Props[Master])

//  for (i <- 1 to 10) {
//    master ! s"[$i] Hello from the world"
//  }

  /**
   * method #2 - a router actor with its own children
   * POOL router
   */


  // 2.1 programmatically (in code)
  val poolMaster = system.actorOf(RoundRobinPool(5).props(Props[Worker]), "simplePoolMaster")

//  for (i <- 1 to 10) {
//    poolMaster ! s"[$i] Hello from the world"
//  }

  // 2.2 configuration
  val poolMaster2 = system.actorOf(FromConfig.props(Props[Worker]), "poolMaster2")
//    for (i <- 1 to 10) {
//      poolMaster2 ! s"[$i] Hello from the world"
//    }

  /**
   * method #3 -  routers with actors created elsewhere
   * GROUP router
   */

  // .. in another part of my application
  val workerList = (1 to 5).map(i => system.actorOf(Props[Worker], s"worker_$i")).toList

  // need their paths
  val workerPaths = workerList.map(workerRef => workerRef.path.toString)

  // 3.1 in  the code
  val groupMaster = system.actorOf(RoundRobinGroup(workerPaths).props())
//  for (i <- 1 to 10) {
//    groupMaster ! s"[$i] Hello from the world"
//  }

  //3.2 - from configuration
  val groupMaster2 = system.actorOf(FromConfig.props(), "groupMaster2")
//  for (i <- 1 to 10) {
//    groupMaster2 ! s"[$i] Hello from the world"
//  }

  /**
   * Special messages
   */
  groupMaster2 ! Broadcast("hello everyone")
  // PoisonPill and Kill are NOT routed
  // AddRoutee, Remove, Get handled only by the routing actor
}
