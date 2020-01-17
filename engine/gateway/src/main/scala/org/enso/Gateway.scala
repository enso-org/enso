package org.enso

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.typesafe.config.ConfigFactory
import org.enso.gateway.protocol.response.Result.InitializeResult
import org.enso.gateway.protocol.{Id, Notifications, Requests, Response}
import org.enso.gateway.protocol.response.result.{
  ServerCapabilities,
  ServerInfo
}

/** The gateway component talks directly to clients using protocol messages,
  * and then handles these messages by talking to the language server.
  *
  * @param languageServer [[ActorRef]] of [[LanguageServer]] actor.
  */
class Gateway(languageServer: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case Requests.Initialize(Id.Number(id), _) =>
      val msg = "Gateway: Initialize received"
      log.info(msg)
      languageServer ! LanguageServer.Initialize(id, sender())

    case LanguageServer.InitializeReceived(id, replyTo) =>
      val msg = "Gateway: InitializeReceived received"
      log.info(msg)
      replyTo ! Response.result(
        id     = Some(Id.Number(id)),
        result = InitializeResult(ServerCapabilities(), Some(serverInfo))
      )

    case Notifications.Initialized(_) =>
      val msg = "Gateway: Initialized received"
      log.info(msg)
      languageServer ! LanguageServer.Initialized

    case LanguageServer.InitializedReceived =>
      val msg = "Gateway: InitializedReceived received"
      log.info(msg)

    case requestOrNotification =>
      val err =
        s"unimplemented request or notification: $requestOrNotification"
      throw new Exception(err)
  }

  private val serverInfo: ServerInfo = {
    val gatewayPath               = "gateway"
    val languageServerPath        = "languageServer"
    val languageServerNamePath    = "name"
    val languageServerVersionPath = "version"
    val gatewayConfig             = ConfigFactory.load.getConfig(gatewayPath)
    val languageServerConfig      = gatewayConfig.getConfig(languageServerPath)
    val name                      = languageServerConfig.getString(languageServerNamePath)
    val version                   = languageServerConfig.getString(languageServerVersionPath)
    ServerInfo(name, Some(version))
  }
}
object Gateway {
  def props(languageServer: ActorRef): Props =
    Props(new Gateway(languageServer))
}
