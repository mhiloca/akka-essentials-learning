package part6patterns

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Cancellable, FSM, Props}
import akka.testkit.{ImplicitSender, TestKit}

import org.scalatest.{BeforeAndAfterAll, OneInstancePerTest}
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

class FSMSpec extends TestKit(ActorSystem("FSMSpec"))
  with ImplicitSender
  with AnyWordSpecLike
  with BeforeAndAfterAll
  with OneInstancePerTest
{
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import FSMSpec._
  import VendingMachineErrors._

  "A vending machine" should {
    runTestSuite(Props[VendingMachine])
  }

  "An FSM vending machine" should {
    runTestSuite(Props[VendingMachineFSM])
  }

  def runTestSuite(props: Props) = {
    "error when not initialized" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! RequestProduct("coke")
      expectMsg(VendingError(MACHINE_NOT_INITIALIZED))
    }

    "report a product not available" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))
      vendingMachine ! RequestProduct("sandwich")
      expectMsg(VendingError(PRODUCT_NOT_AVAILABLE))
    }

    "throw a timeout if I don't insert money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("please insert 1 dollars"))

      within (1.5 seconds) {
        expectMsg(VendingError(REQUEST_TIMED_OUT))
      }
    }

    "handle the reception of partial money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("please insert 3 dollars"))

      vendingMachine ! ReceiveMoney(1)
      expectMsg(Instruction("please insert 2 dollars"))

      within(1.5 seconds) {
        expectMsg(VendingError(REQUEST_TIMED_OUT))
        expectMsg(GiveBackChange(1))
      }
    }

    "deliver the product if I insert all the money" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("please insert 3 dollars"))

      vendingMachine ! ReceiveMoney(3)
      expectMsg(Deliver("coke"))
    }

    "give back change and be able to request money for a new product" in {
      val vendingMachine = system.actorOf(props)
      vendingMachine ! Initialize(Map("coke" -> 10), Map("coke" -> 3))
      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("please insert 3 dollars"))

      vendingMachine ! ReceiveMoney(4)
      expectMsg(Deliver("coke"))
      expectMsg(GiveBackChange(1))

      vendingMachine ! RequestProduct("coke")
      expectMsg(Instruction("please insert 3 dollars"))
    }
  }
}

object FSMSpec {

  /*
    Vending Machine
   */

  case object VendingMachineErrors {
    val MACHINE_NOT_INITIALIZED = "MachineNotInitializedError"
    val PRODUCT_NOT_AVAILABLE = "ProductNotAvailable"
    val REQUEST_TIMED_OUT = "RequestTimedOut"
  }

  case class Initialize(inventory: Map[String, Int], prices: Map[String, Int])
  case class RequestProduct(product: String)

  case class Instruction(instruction: String) // msg the VM will show on its "screen"
  case class ReceiveMoney(amount: Int)
  case class Deliver(product: String)
  case class GiveBackChange(amount: Int)

  case class VendingError(message: String)
  case object ReceiveMoneyTimeout

  class VendingMachine extends Actor with ActorLogging {
    import VendingMachineErrors._

    implicit val executionContext: ExecutionContext = context.dispatcher

    override def receive: Receive = idle

    def idle: Receive = {
      case Initialize(inventory, prices) => context.become(operational(inventory, prices))
      case _ => sender() ! VendingError(MACHINE_NOT_INITIALIZED)
    }

    def operational(inventory: Map[String, Int], prices: Map[String, Int]): Receive = {
      case RequestProduct(product) => inventory.get(product) match {
        case None | Some(0) => sender() ! VendingError(PRODUCT_NOT_AVAILABLE)
        case Some(_) =>
          val price = prices(product)
          sender() ! Instruction(s"please insert $price dollars")
          context.become(waitForMoney(inventory, prices, product, 0, startReceiveMoneyTimeoutSchedule, sender()))
      }
    }

    def waitForMoney(
                      inventory: Map[String, Int],
                      prices: Map[String, Int],
                      product: String,
                      money: Int,
                      moneyTimeoutSchedule: Cancellable,
                      requester: ActorRef
                    ): Receive = {
      case ReceiveMoneyTimeout =>
        requester ! VendingError(REQUEST_TIMED_OUT)
        if (money > 0) requester ! GiveBackChange(money)
        context.become(operational(inventory, prices))
      case ReceiveMoney(amount) =>
        moneyTimeoutSchedule.cancel()
        val price = prices(product)
        if (money + amount >= price) {
          // user buys the product
          requester ! Deliver(product)

          // machine delivers the change
          val change = (money + amount) - price
          if (change > 0) requester ! GiveBackChange(change)

          // updating inventory
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          context.become(operational(newInventory, prices))
        } else  {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"please insert $remainingMoney dollars")
          context.become(waitForMoney(
            inventory, prices, product, money + amount, startReceiveMoneyTimeoutSchedule, requester
          ))
        }

    }

    def startReceiveMoneyTimeoutSchedule: Cancellable = context.system.scheduler.scheduleOnce(1 second){
      self ! ReceiveMoneyTimeout
    }
  }

  // step 1 - define the states and the data of the actor
  /*
    - the states are the message handlers
    - the data is the set of parameters for these receive message handlers
   */

  trait VendingState
  case object Idle extends VendingState
  case object Operational extends VendingState
  case object WaitForMoney extends VendingState

  trait VendingData
  case object Uninitialized extends VendingData
  case class Initialized(inventory: Map[String, Int], prices: Map[String, Int]) extends VendingData
  case class WaitForMoneyData(
                               inventory: Map[String, Int],
                               prices: Map[String, Int],
                               product: String,
                               money: Int,
                               requester: ActorRef
                             ) extends VendingData
  class VendingMachineFSM extends FSM[VendingState, VendingData] {
    import VendingMachineErrors._

    // we don't have receive handlers

    // an EVENT(message, data)

    /*
      state, data

      event => state and data can be changed.

      state = Idle
      data = Uninitialized

      event(Initialize(Map("coke" -> 10), Map("coke" -> 1)))
        =>
        state = Operational
        data = WaitForMoneyData(Map(coke -> 10), Map("coke" -> 1), coke, 0, R)

      event(ReceiveMoney(2), WaitForMoneyData(Map(coke -> 10), Map("coke" -> 1), coke, 0, R))
        =>
        state = Operational
        data = Initialized(Map("coke" -> 9), Map("coke" -> 1))

     */

    startWith(Idle, Uninitialized)

    when(Idle) {
      case Event(Initialize(inventory, prices), Uninitialized) =>
        goto(Operational) using Initialized(inventory, prices)
        // equivalent with context.become(operational(inventory, prices))
      case _ =>
        sender() ! VendingError(MACHINE_NOT_INITIALIZED)
        stay()
    }

    when(Operational) {
      case Event(RequestProduct(product), Initialized(inventory, prices)) =>
        inventory.get(product) match {
          case None | Some(0) =>
            sender() ! VendingError(PRODUCT_NOT_AVAILABLE)
            stay()
          case Some(_) =>
            val price = prices(product)
            sender() ! Instruction(s"please insert $price dollars")
            goto(WaitForMoney) using WaitForMoneyData(inventory, prices, product, 0, sender())
        }
    }

    when(WaitForMoney, stateTimeout = 1 second) {
      case Event(StateTimeout, WaitForMoneyData(inventory, prices, product, money, requester)) =>
        requester ! VendingError(REQUEST_TIMED_OUT)
        if (money > 0) requester ! GiveBackChange(money)
        goto(Operational) using Initialized(inventory, prices)

      case Event(ReceiveMoney(amount), WaitForMoneyData(inventory, prices, product, money, requester)) =>
        val price = prices(product)
        if (money + amount >= price) {
          // user buys the product
          requester ! Deliver(product)

          // machine delivers the change
          val change = (money + amount) - price
          if (change > 0) requester ! GiveBackChange(change)

          // updating inventory
          val newStock = inventory(product) - 1
          val newInventory = inventory + (product -> newStock)
          goto(Operational) using Initialized(newInventory, prices)

        } else  {
          val remainingMoney = price - money - amount
          requester ! Instruction(s"please insert $remainingMoney dollars")
          stay() using WaitForMoneyData(inventory, prices, product, money + amount, requester)
        }
    }

    whenUnhandled {
      case Event(_,_) =>
        sender() ! VendingError("CommandNotFound")
        stay()
    }

    onTransition {
      case stateA -> stateB => log.info(s"Transition from $stateA to $stateB")
    }

    initialize()

  }
}
