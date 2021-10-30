package org.enso.languageserver.monitoring

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.monitoring.HealthCheckEndpointSpec.{
  DeadSubsystem,
  LiveSubsystem
}
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.requesthandler.monitoring.PingHandler
import org.enso.testkit.FlakySpec
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import scala.concurrent.duration._

class HealthCheckEndpointSpec
    extends AnyFlatSpecLike
    with Matchers
    with FlakySpec
    with ScalatestRouteTest
    with Directives {

  implicit val timeout = RouteTestTimeout(25.seconds)

  "A readiness probe" should "reply with 200 when the Language Server is ready to accept requests" in new TestCtx {
    //given
    val liveSubsystem = system.actorOf(Props(new LiveSubsystem))
    override def livenessProbeProps: Props =
      PingHandler.props(List(liveSubsystem), 10.seconds)

    /*
     * The objectUnderTest must be a lazy val. In order to initialize
     * actors created by this class before publishing events I had to
     * artificially call route method.
     */
    objectUnderTest.route
    //when
    Thread.sleep(1000)
    system.eventStream.publish(InitializedEvent.InitializationFinished)
    Thread.sleep(1000)
    Get("/_health/readiness") ~> objectUnderTest.route ~> check {
      //then
      status mustEqual StatusCodes.OK
    }
  }

  it should "reply with 500 when the Language Server is not initialized" in new TestCtx {
    //given
    val liveSubsystem = system.actorOf(Props(new LiveSubsystem))
    override def livenessProbeProps: Props =
      PingHandler.props(List(liveSubsystem), 10.seconds)
    //when
    Thread.sleep(1000)
    Get("/_health/readiness") ~> objectUnderTest.route ~> check {
      //then
      status mustEqual StatusCodes.InternalServerError
    }
  }

  "A liveness probe" should "reply with 200 when all subsystems are up and running" in new TestCtx {
    //given
    val liveSubsystem = system.actorOf(Props(new LiveSubsystem))
    override def livenessProbeProps: Props =
      PingHandler.props(List(liveSubsystem), 10.seconds, true)
    //when
    Get("/_health/liveness") ~> objectUnderTest.route ~> check {
      //then
      status mustEqual StatusCodes.OK
    }
  }

  it should "reply with 500 when any subsystem is dead" in new TestCtx {
    //given
    val liveSubsystem = system.actorOf(Props(new LiveSubsystem))
    val deadSubsystem = system.actorOf(Props(new DeadSubsystem))
    override def livenessProbeProps: Props =
      PingHandler.props(List(liveSubsystem, deadSubsystem), 10.seconds, true)
    //when
    Get("/_health/liveness") ~> objectUnderTest.route ~> check {
      //then
      status mustEqual StatusCodes.InternalServerError
    }
  }

  trait TestCtx {

    def livenessProbeProps: Props

    lazy val objectUnderTest =
      new HealthCheckEndpoint(livenessProbeProps, system)

  }

}

object HealthCheckEndpointSpec {

  class LiveSubsystem extends Actor {
    override def receive: Receive = { case Ping => sender() ! Pong }
  }

  class DeadSubsystem extends Actor {
    override def receive: Receive = { case _ => () }
  }

}
