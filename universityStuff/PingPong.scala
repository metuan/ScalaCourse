

import akka.actor._

case class StartMsg(counter: Int)
case object EndMsg
case class PingMsg(counter: Int)
case class PongMsg(counter: Int)

class Player extends Actor {
  def receive = {
    case StartMsg(k) => {
      println("Begin game");
      println("ping");
      sender ! (PingMsg(k))
    }
    case EndMsg => {
      println("End of game");
      context.system.shutdown
    }
    case PingMsg(k) => {
      println("pong");
      if (k - 1 <= 0)
        sender ! EndMsg
      else
        sender ! (PongMsg(k - 1))
    }
    case PongMsg(k) => {
      println("ping");
      if (k - 1 <= 0)
        sender ! EndMsg
      else
        sender ! (PingMsg(k - 1))
    }
    case _ => println("Wrong request")
  }
}

object Main {
  def main(args: Array[String]) {
    val system = ActorSystem("MySystem");
    val player1 = system.actorOf(Props[Player])
    val player2 = system.actorOf(Props[Player])
    player1.tell(StartMsg(5), player2);
  }
}