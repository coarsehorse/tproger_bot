package controllers

import javax.inject.Inject
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import scala.concurrent.Future
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global

class Facebook @Inject() (ws: WSClient) extends Controller {
  /**
    * Confirmation for Facebook that connection is ok
    */
  def webhook = Action { req =>
    val VALIDATION_TOKEN = sys.env("VALIDATION_TOKEN")

    val mode = req.getQueryString("hub.mode")
    val verifyToken = req.getQueryString("hub.verify_token")

    (mode, verifyToken) match {
      case (Some("subscribe"), Some(VALIDATION_TOKEN)) =>
        Ok(req.getQueryString("hub.challenge").get)
      case _ =>
        Status(403)
    }
  }

  /**
    * Handle Facebook POST request
    */
  def receiveMessage = Action(parse.tolerantJson) { req =>
    val data = req.body

    (data \ "object").as[String] match {

      // Make sure this is a page subscription
      case "page" =>
        Try {
        	val entry = (data \ "entry").as[JsArray]
        	val messaging = (entry(0) \ "messaging").as[JsArray]
        	val senderId = (messaging(0) \ "sender" \ "id").as[String]
        	val message = (messaging(0) \ "message" \ "text").as[String]
        	(senderId, message)
        } match {
        	case Success(s) =>
        		commandHandler(s._1, s._2)
        	case Failure(f) =>
        		println("JSON:\n" + Json.prettyPrint(data))
        		println("Exception:\n" + f)
        }

        // We must send back a 200, within 20 seconds, to let Facebook know we've
        // successfully received the callback. Otherwise, the request will time out.
        Status(200)

      case _ =>
        Status(403)
    }
  }

  /**
    * Handles user command from message
    * @param senderId user id
    * @param message user message
    */
  private def commandHandler(senderId: String, message: String) = {
    val command = """(\w+) tag ([А-Яа-я\w\d-]+)""".r // unfortunately simple \w not matches cyrillic

    message match {
      case command("find", tag) =>
        Future[List[(String, List[String])]] {
          utils.Parser.searchFor(tag)
        } onComplete {
          case Success(art_tags) =>
            if (art_tags.length == 0)
              sendTextMessage(senderId, s"No articles by tag '$tag' was found on tproger.ru")
            else {
              val str_links = art_tags map (_._1) mkString("\n") // get only URLs

              sendTextMessage(senderId,
                s"Found articles by tag '$tag':\n"
                  + str_links + "\n"
                  + "This will be saved to the DB")

              for { // write to the DB asynchronously
                a_t <- art_tags
                t <- a_t._2
              } yield Future {
                utils.DB.addNewTagArticle(t, a_t._1)
              } onFailure {
                case e =>
                  println(e)
                  sendTextMessage(senderId,
                    "Error occurs with writing to the DB\n"
                      + s"data: tag='$t', url='${a_t._1}'\n"
                      + "Error message:   "
                      + e.getMessage
                      + "\nPlease, send this message to developer")
              }
            }
          case Failure(e) =>
            println(e)
            sendTextMessage(senderId,
              "Sorry, there was a problem:\n"
                + e.getMessage
                + "\nPlease, send this message to developer")
        }

      case command("show", tag) =>
        Future[List[String]] {
          utils.DB.getArticlesByTag(tag)
        } onComplete {
          case Success(articles) =>
            if (articles.length == 0)
              sendTextMessage(senderId, s"No saved articles by tag '$tag'")
            else
              sendTextMessage(senderId, s"Saved articles by tag '$tag':\n" + articles.mkString("\n"))
          case Failure(e) =>
            println(e)
            sendTextMessage(senderId,
              "Sorry, there was a problem:\n"
                + e.getMessage
                + "\nPlease, send this message to developer")
        }

      case "help" =>
        sendTextMessage(senderId,
          """Available commands:
               find tag tagValue
                 - search by tagValue on tproger.ru
               show tag tagValue
                 - show saved(in DB) articles by tagValue
               about
                 - about this bot
               help
                 - this message
               P.S. tagValue - any available tag
               satisfying regex [\w\d-]+
               Example:
               show tag scala
                  """)

      case "about" =>
        val aboutMessage = "This bot was completed within the framework of the dataRoot internship. It's open source project under the MIT license. Feel free to learn/fork/pule on Github: https://github.com/heroys6/tproger_bot"
        sendTextMessage(senderId, aboutMessage)

      case _ =>
        sendTextMessage(senderId, "Command not recognized. Type \"help\" for more information")
    }
  }

  /**
    * Send message to specified recipient
    * @param recipientID reply to this user
    * @param messageText bot reply text
    */
  def sendTextMessage(recipientID: String, messageText: String) = {
    val ACCESS_TOKEN = sys.env("ACCESS_TOKEN")

    ws.url("https://graph.facebook.com/v2.6/me/messages")
      .withQueryString("access_token" -> ACCESS_TOKEN)
      .post(Json.obj(
        "recipient" -> Json.obj("id" -> recipientID),
        "message" -> Json.obj("text" -> messageText)
      ))
  }
}
