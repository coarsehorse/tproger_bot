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
        println("Validating webhook")
        Ok(req.getQueryString("hub.challenge").get)
      case _ =>
        println("Failed validation. Make sure the validation tokens match.")
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
        /**
          * {{{
          * {
          *   "object":"page",
          *   "entry":[
          *     {
          *       "id":"PAGE_ID",
          *       "time":1458692752478,
          *       "messaging":[
          *         {
          *           "sender":{
          *             "id":"USER_ID"
          *           },
          *           "recipient":{
          *             "id":"PAGE_ID"
          *           },
          *
          *           ...
          *         }
          *       ]
          *     }
          *   ]
          * }
          * }}}
          */

        val entries = (data \ "entry").as[List[JsValue]]
        entries.foreach { pageEntry =>
          //val pageID = (pageEntry \ "id").as[Long]
          //val timeOfEvent = (pageEntry \ "time").as[Long]

          val messaging = (pageEntry \ "messaging").as[List[JsObject]]
          messaging.foreach { messagingEvent =>
            Logger.debug(s"\nHey doc, see what I've found: $messagingEvent\n")
            receivedMessage(messagingEvent)
          }
        }

        // Assume all went well.
        //
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
    println {
      "Received message for user %s and page %s with message: %s".format(senderID, recipientID, maybeMessage)
    }

    maybeMessage.foreach { message =>
      (message \ "text").asOpt[String].foreach { messageText =>
        sendTextMessage(senderID, messageText)
      }
    }
  }

  def sendTextMessage(recipientID: String, messageText: String) = {
    val ACCESS_TOKEN = sys.env("ACCESS_TOKEN")

    ws.url("https://graph.facebook.com/v2.6/me/messages")
      .withQueryString("access_token" -> ACCESS_TOKEN)
      .post(Json.obj(
        "recipient" -> Json.obj("id" -> recipientID),
        "message" -> Json.obj("text" -> messageText.toUpperCase)
      ))
  }
}
