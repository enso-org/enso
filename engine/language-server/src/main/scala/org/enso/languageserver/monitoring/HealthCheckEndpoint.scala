package org.enso.languageserver.monitoring

import java.util.UUID

import akka.actor.{ActorRef, ActorRefFactory, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import org.enso.jsonrpc._
import org.enso.languageserver.monitoring.MonitoringApi.InitialPing

import scala.annotation.unused
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class HealthCheckEndpoint(
  initialPingProps: Props,
  pingHandlerProps: Props,
  actorFactory: ActorRefFactory
)(implicit
  ec: ExecutionContext
) extends Endpoint {

  implicit private val timeout: Timeout = Timeout(10.seconds)

  private val initialPingHandler =
    actorFactory.actorOf(initialPingProps, "readiness-probe")

  override def route: Route =
    readinessProbe ~ livenessProbe ~ classicalHealthCheck

  private val readinessProbe =
    path("_health" / "readiness") {
      head {
        checkReadiness()
      } ~
      get {
        checkReadiness()
      }
    }

  private val livenessProbe =
    path("_health" / "liveness") {
      head {
        checkLiveness()
      } ~
      get {
        checkLiveness()
      }
    }

  private val classicalHealthCheck =
    path("_health") {
      head {
        complete("OK")
      } ~
      get {
        complete("OK")
      }
    }

  private def checkReadiness(): Route =
    askHandler(initialPingHandler, InitialPing)

  private def checkLiveness(): Route = {
    val pingHandler =
      actorFactory.actorOf(pingHandlerProps, UUID.randomUUID().toString)

    askHandler(pingHandler, MonitoringApi.Ping)
  }

  private def askHandler[M <: Method](handler: ActorRef, method: M)(implicit
    @unused ev: HasParams.Aux[M, Unused.type]
  ): Route = {
    val requestId = Id.String(UUID.randomUUID().toString)
    val future =
      (handler ? Request(method, requestId, Unused))
        .flatMap {
          case ResponseResult(`method`, `requestId`, Unused) =>
            Future.successful(())

          case ResponseError(Some(`requestId`), _) =>
            Future.failed(
              new Exception("Language Server has not been initialized yet")
            )
        }

    onComplete(future) {
      case Failure(_) =>
        complete(StatusCodes.InternalServerError)

      case Success(()) =>
        complete(StatusCodes.OK)
    }
  }

}
