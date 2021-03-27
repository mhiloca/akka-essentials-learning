package part2actors

import akka.actor.{Actor, ActorSystem, Props}

object ActorsIntro extends App {

  // part1 - actor system
  val actorSystem = ActorSystem("firstActorSystem")
  println(actorSystem.name)

  // part2 - create actors
  // word count actor
  class WordCountActor extends Actor {
    //internal data
    var totalWords = 0

    // behaviours
    def receive: PartialFunction[Any, Unit] = {
      case message: String =>
        println(s"[word counter] I have received: $message")
        totalWords += message.split(" ").length
        println(s"[word counter] It has $totalWords words")
      case msg => println(s"[word counter] I cannot understand ${msg.toString}")
    }
  }

  // part3
  val wordCounter = actorSystem.actorOf(Props[WordCountActor], "wordCounter")
  val anotherWordCounter = actorSystem.actorOf(Props[WordCountActor], "anotherWordCounter")

  // part4 - communicate!
  wordCounter ! "I am learning AKKA and it's pretty damn cool!" // infix notation of "tell"
  anotherWordCounter ! "I really enjoy when I get to do things on my own"

  class Person(name: String) extends Actor {
    override def receive: Receive = {
      case "hi" => println(s"[person] Hi, my name is $name")
      case _ => println(s"[person] Sorry, I don't understand")
    }
  }
  object Person {
    def props(name: String) = Props(new Person(name))
  }

  val person = actorSystem.actorOf(Person.props("Bob"), "personBob")
  person ! "hi"
  person ! "whatever"


}
