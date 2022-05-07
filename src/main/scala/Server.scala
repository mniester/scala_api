import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import scala.io.StdIn

import Routes._
import Settings._

object Server {

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem(Behaviors.empty, "main")
    implicit val executionContext = system.executionContext

    val bindingFuture = Http().newServerAt(localHostName, port).bind(allRoutes)

    println(s"Server now online. Please navigate to http://localhost:8080/hello\nPress RETURN to stop...")
    StdIn.readLine()
    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}