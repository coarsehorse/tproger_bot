package controllers

import javax.inject.Inject

import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.mvc._
import play.api.Logger

class Facebook @Inject() (ws: WSClient) extends Controller {
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

  def receiveMessage = Action(parse.tolerantJson) { req =>
    val data = req.body

    (data \ "object").as[String] match {
      // Make sure this is a page subscription
      case "page" =>
        // Iterate over each entry
        // There may be multiple if batched
        val entries = (data \ "entry").as[List[JsValue]]
        
        entries.foreach { pageEntry =>
          //val pageID = (pageEntry \ "id").as[Long]
          //val timeOfEvent = (pageEntry \ "time").as[Long]
          val messaging = (pageEntry \ "messaging").as[List[JsObject]]
          
          messaging.foreach { messagingEvent =>
            receivedMessage(messagingEvent)
          }
        }

        // You must send back a 200, within 20 seconds, to let us know you've
        // successfully received the callback. Otherwise, the request will time out.
        Status(200)
      case _ =>
        Status(403)
    }
  }

  private def receivedMessage(event: JsObject) = {
    val senderID = (event \ "sender" \ "id").as[String]
    val recipientID = (event \ "recipient" \ "id").as[String]
    val maybeMessage = (event \ "message").asOpt[JsObject]

    maybeMessage.foreach { message =>
      (message \ "text").asOpt[String].foreach { messageText =>
        val command = """(\w+) tag ([\w\d-]+)""".r

        messageText match {
          case command("find", tag) =>
            sendTextMessage(senderID, s"I will search in web by tag: '$tag'")
          case command("show", tag) =>
            sendTextMessage(senderID, s"I will show from db by tag: '$tag'")
          case "help" =>
            sendTextMessage(senderID,
            """Available commands:

                > **"find tag $tag"**
                  - search for articles by $tag
                > **"show tag $tag"**
                  - show saved articles by $tag
               where **$tag**
                 - any available tag
                  taht's satisfy regex: [\w\d-]+ )""")
          case _ =>
            sendTextMessage(senderID, "Command not recognized. Type \"help\" for more information")
        }
      }
    }
  }

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
