package org.enso.languageserver.requesthandler.monitoring

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.enso.jsonrpc.Id.Number
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.monitoring.MonitoringApi
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import scala.concurrent.duration._

class PingHandlerSpec
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyFlatSpecLike
    with Matchers {

  "A PingHandler" must "scatter pings to all subsystems" in {
    //given
    val subsystem1 = TestProbe()
    val subsystem2 = TestProbe()
    val subsystem3 = TestProbe()
    val actorUnderTest = system.actorOf(
      PingHandler
        .props(List(subsystem1.ref, subsystem2.ref, subsystem3.ref), 10.seconds)
    )
    //when
    actorUnderTest ! Request(MonitoringApi.Ping, Number(1), Unused)
    //then
    subsystem1.expectMsg(Ping)
    subsystem2.expectMsg(Ping)
    subsystem3.expectMsg(Ping)
    //teardown
    system.stop(actorUnderTest)
  }

  it must "gather Pong messages and reply with success response" in {
    //given
    val subsystem1 = TestProbe()
    val subsystem2 = TestProbe()
    val subsystem3 = TestProbe()
    val actorUnderTest = system.actorOf(
      PingHandler
        .props(List(subsystem1.ref, subsystem2.ref, subsystem3.ref), 10.seconds)
    )
    //when
    actorUnderTest ! Request(MonitoringApi.Ping, Number(1), Unused)
    subsystem1.expectMsg(Ping)
    subsystem1.lastSender ! Pong
    subsystem2.expectMsg(Ping)
    subsystem2.lastSender ! Pong
    subsystem3.expectMsg(Ping)
    subsystem3.lastSender ! Pong
    //then
    expectMsg(ResponseResult(MonitoringApi.Ping, Number(1), Unused))
    //teardown
    system.stop(actorUnderTest)
  }

  it must "stop without replying when some of subsystems don't reply on time" in {
    //given
    val subsystem1 = TestProbe()
    val subsystem2 = TestProbe()
    val subsystem3 = TestProbe()
    val actorUnderTest = system.actorOf(
      PingHandler
        .props(List(subsystem1.ref, subsystem2.ref, subsystem3.ref), 2.seconds)
    )
    watch(actorUnderTest)
    //when
    actorUnderTest ! Request(MonitoringApi.Ping, Number(1), Unused)
    subsystem2.expectMsg(Ping)
    subsystem2.lastSender ! Pong
    subsystem3.expectMsg(Ping)
    subsystem3.lastSender ! Pong
    //then
    expectTerminated(actorUnderTest)
    expectNoMessage()
    //teardown
    system.stop(actorUnderTest)
  }

}
