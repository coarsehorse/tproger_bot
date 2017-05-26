import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import co.datamonsters.facebot._
import co.datamonsters.facebot.api.{Message, Messaging}
import co.datamonsters.facebot.api.NotificationType.Regular
import scala.io.StdIn

object runDebug {
  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem("facebotActorSystem")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    val facebotConnection = AkkaHttpConnection(Credentials(
      "EAAQPrezwkBkBANKacNttTxepZAJa0bZBPIZBQjHFo3q4bmgWWxFbZAJcZAF7TrgrVJxN0x3JgY5wo43FutPk9UTGe2TMgmOg3TwJfIMwRzEeZBDAQmrs91o8EOwRDGWkudZCBpE5S2d5YiFWM8y5hvRre3yV10jkjXIkA6QReJfYCgteZBcyOS2Y",
      "EAAVe5KsNKE8BAJZCFQ1jOd0ePtI1geKTXZCp3t7BvjnKCn4Mpq36cZCROuNYKxposPJWIMqJl0vZBZClfgfwzU8RZATx3kZCIyBNvDLoUULgXXP08WUJZBa60SPLPnNlRZAOASGUdctawlCk4rDi341aOYg172rZCasdPUM4ZCW1sMn3mQbz0ZA143sZC"
      )) {
      case sendApi @ Event(_, Messaging.MessageReceived(sender, _, message))
          if !message.isEcho =>
        sendApi.sendMessage(sender, Message("MESSAGE_TEXT"), Regular)
    }

    val bindingFuture =
      Http().bindAndHandle(facebotConnection.facebookRestRoute, "https://hidden-oasis-82174.herokuapp.com/")
    /*StdIn.readLine() // let it run until user presses return
    bindingFuture.flatMap(_.unbind()).onComplete(_ => system.terminate())*/

  }
}