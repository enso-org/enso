package org.enso.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import org.enso.languageserver.jsonrpc.{
  Error,
  HasParams,
  HasResult,
  MessageHandler,
  Method,
  Notification,
  Protocol,
  Request,
  ResponseError,
  ResponseResult,
  Unused
}

import scala.concurrent.ExecutionContext
import scala.util.Success
import akka.util.Timeout
import akka.pattern.ask
import io.circe.{Decoder, Encoder}

import scala.concurrent.duration._

object JsonRpcApi {
  case object CantCompleteRequestError
      extends Error(1, "Can't complete request")

  sealed abstract class Capability(method: String)
  case class CanEdit(path: String) extends Capability("canEdit")

  object Capability {
    import cats.syntax.functor._
    import io.circe.generic.auto._
    import io.circe.syntax._

    implicit val encoder: Encoder[Capability] = {
      case cap: CanEdit => cap.asJson
    }

    implicit val decoder: Decoder[Capability] = Decoder[CanEdit].widen
  }

  case class CapabilityRegistration(id: UUID, capability: Capability)

//  object CapabilityRegistration {
//    implicit val encoder: Encoder[CapabilityRegistration] = Json.
//  }

  case object AcquireWriteLock extends Method("acquireWriteLock") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ForceReleaseWriteLock extends Method("forceReleaseWriteLock") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
  }

  val protocol: Protocol = Protocol.empty
    .registerRequest(AcquireWriteLock)
    .registerNotification(ForceReleaseWriteLock)
    .registerError(CantCompleteRequestError)

  case class WsConnect(webActor: ActorRef)
}

class Client(val clientId: LanguageProtocol.ClientId, val server: ActorRef)
    extends Actor
    with Stash
    with ActorLogging {
  implicit val timeout: Timeout     = 5.seconds
  implicit val ec: ExecutionContext = context.dispatcher

  override def receive: Receive = {
    case JsonRpcApi.WsConnect(webActor) =>
      log.debug("WebSocket connected.")
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      log.debug("WebSocket disconnected.")
      server ! LanguageProtocol.Disconnect(clientId)
      context.stop(self)
    case Request(JsonRpcApi.AcquireWriteLock, id, Unused) =>
      (server ? LanguageProtocol.AcquireWriteLock(clientId)).onComplete {
        case Success(LanguageProtocol.WriteLockAcquired) =>
          webActor ! ResponseResult(JsonRpcApi.AcquireWriteLock, id, Unused)
        case _ =>
          webActor ! ResponseError(
            Some(id),
            JsonRpcApi.CantCompleteRequestError
          )
      }
    case LanguageProtocol.ForceReleaseWriteLock =>
      webActor ! Notification(JsonRpcApi.ForceReleaseWriteLock, Unused)
  }
}
