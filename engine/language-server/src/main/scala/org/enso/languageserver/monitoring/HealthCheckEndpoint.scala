package org.enso.languageserver.monitoring

import java.util.UUID

import akka.actor.{ActorRefFactory, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.util.Timeout
import org.enso.jsonrpc._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** HTTP endpoint that provides health checking capabilities.
  *
  * @param pingHandlerProps a configuration object used to create ping handler actors
  * @param actorFactory a factory used to create actors
  * @param ec an execution context
  */
class HealthCheckEndpoint(
  pingHandlerProps: Props,
  actorFactory: ActorRefFactory
)(implicit ec: ExecutionContext)
    extends Endpoint {

  implicit private val timeout: Timeout = Timeout(10.seconds)

  private val readinessMonitor =
    actorFactory.actorOf(
      ReadinessMonitor.props(),
      s"readiness-probe-${UUID.randomUUID()}"
    )

  /** @inheritdoc */
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

  private def checkReadiness(): Route = {
    val future =
      (readinessMonitor ? MonitoringProtocol.IsReady)
        .flatMap {
          case MonitoringProtocol.OK =>
            Future.successful(())

          case MonitoringProtocol.KO =>
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

  private def checkLiveness(): Route = {
    val pingHandler =
      actorFactory.actorOf(pingHandlerProps, UUID.randomUUID().toString)

    val requestId = Id.String(UUID.randomUUID().toString)
    val future =
      (pingHandler ? Request(MonitoringApi.Ping, requestId, Unused))
        .flatMap {
          case ResponseResult(MonitoringApi.Ping, `requestId`, Unused) =>
            Future.successful(())

          case ResponseError(Some(`requestId`), _) =>
            Future.failed(
              new Exception("Healthiness check failed")
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
