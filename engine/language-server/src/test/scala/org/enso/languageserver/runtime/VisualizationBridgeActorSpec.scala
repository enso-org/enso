package org.enso.languageserver.runtime

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import org.enso.languageserver.runtime.VisualizationBridgeServer._
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.testkit.ReportLogsOnFailure
import org.enso.ydoc.api.YjsChannel
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.must.Matchers

import java.nio.ByteBuffer
import java.util.UUID
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.function.Consumer

class VisualizationBridgeActorSpec
    extends TestKit(ActorSystem("VisualizationBridgeActorSpec"))
    with ImplicitSender
    with AnyFlatSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ReportLogsOnFailure {

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
  }

  /** In-memory stand-in for a `YjsChannel` so tests can assert outbound
    * messages without running the full GraalVM + Y.Array plumbing.
    */
  final private class RecordingChannel extends YjsChannel {
    val sent: ConcurrentLinkedQueue[Object] =
      new ConcurrentLinkedQueue[Object]()

    override def send(message: Object): Unit = sent.add(message)

    override def subscribe(messageHandler: Consumer[Object]): Unit = ()
  }

  private def pollSentAsString(
    ch: RecordingChannel,
    waitMs: Long = 2000
  ): String = {
    val deadline = System.currentTimeMillis() + waitMs
    while (System.currentTimeMillis() < deadline) {
      val msg = ch.sent.poll()
      if (msg != null) return msg.asInstanceOf[String]
      Thread.sleep(5)
    }
    fail(s"No message sent on channel within ${waitMs}ms")
  }

  private def pollSentAsBuffer(
    ch: RecordingChannel,
    waitMs: Long = 2000
  ): ByteBuffer = {
    val deadline = System.currentTimeMillis() + waitMs
    while (System.currentTimeMillis() < deadline) {
      val msg = ch.sent.poll()
      if (msg.isInstanceOf[ByteBuffer]) return msg.asInstanceOf[ByteBuffer]
      if (msg != null) {
        // Skip non-ByteBuffer (e.g. ready JSON) messages.
      } else {
        Thread.sleep(5)
      }
    }
    fail(s"No binary frame sent on channel within ${waitMs}ms")
  }

  private def drainAllSent(ch: RecordingChannel): List[Object] = {
    val out  = new java.util.ArrayList[Object]()
    var next = ch.sent.poll()
    while (next != null) {
      out.add(next)
      next = ch.sent.poll()
    }
    import scala.jdk.CollectionConverters._
    out.asScala.toList
  }

  "VisualizationBridgeActor" must "forward attach control messages as Api.AttachVisualization" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID().toString
    val contextId       = UUID.randomUUID().toString
    val nodeId          = UUID.randomUUID().toString
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity","positionalArgumentsExpressions":["a"]}}""".stripMargin

    actor ! ControlMessage(attachJson)

    val fwd = runtime.expectMsgType[Api.Request]
    fwd.payload.isInstanceOf[Api.AttachVisualization] must be(true)
    val att = fwd.payload.asInstanceOf[Api.AttachVisualization]
    att.visualizationId.toString must be(visualizationId)
    att.expressionId.toString must be(nodeId)
    att.visualizationConfig.executionContextId.toString must be(contextId)
    att.visualizationConfig.visualizationModule must be("M")

    system.stop(actor)
  }

  it must "emit a binary data frame + ready message when the runtime emits a VisualizationUpdate for a tracked visualization" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    actor ! ControlMessage(attachJson)
    runtime.expectMsgType[Api.Request]

    // Flush the attach echo (if any) from the control channel so the test
    // below only observes the `ready` message.
    drainAllSent(control)

    // Simulate the runtime producing an update.
    val bytes = Array[Byte](1, 2, 3, 4, 5)
    system.eventStream.publish(
      Api.VisualizationUpdate(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        bytes
      )
    )

    // A `ready` control message and a binary frame should land on the
    // respective channels. Poll briefly to account for eventStream dispatch
    // latency.
    val t0 = System.currentTimeMillis()
    while (
      (data.sent.isEmpty || control.sent.isEmpty)
      && System.currentTimeMillis() - t0 < 2000
    ) { Thread.sleep(10) }

    val readyMsg = pollSentAsString(control)
    readyMsg must include(""""kind":"ready"""")
    readyMsg must include(requestId)

    val frame = pollSentAsBuffer(data)
    frame.remaining() must be(16 + bytes.length)

    // The first 16 bytes are the request id as a UUID; the rest is the payload.
    val msb  = frame.getLong(0)
    val lsb  = frame.getLong(8)
    val uuid = new UUID(msb, lsb)
    uuid.toString must be(requestId)

    val payload = new Array[Byte](bytes.length)
    val dup     = frame.duplicate()
    dup.position(16)
    dup.get(payload)
    payload must be(bytes)

    system.stop(actor)
  }

  it must "emit a failed control message on VisualizationEvaluationFailed" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    actor ! ControlMessage(attachJson)
    runtime.expectMsgType[Api.Request]
    drainAllSent(control)

    system.eventStream.publish(
      Api.VisualizationEvaluationFailed(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        "boom",
        None
      )
    )

    // Poll briefly.
    val t0 = System.currentTimeMillis()
    while (control.sent.isEmpty && System.currentTimeMillis() - t0 < 2000) {
      Thread.sleep(10)
    }

    val msg = pollSentAsString(control)
    msg must include(""""kind":"failed"""")
    msg must include("boom")
    msg must include(requestId)

    system.stop(actor)
  }

  it must "forward attach with an inFrame expression as Api.AttachVisualization with VisualizationExpression.InFrame" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID().toString
    val contextId       = UUID.randomUUID().toString
    val nodeId          = UUID.randomUUID().toString
    val expression      = "1 + 2"
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId",
         |"request":{"visualizationModule":"",
         |"expression":{"inFrame":"$expression"}}}""".stripMargin

    actor ! ControlMessage(attachJson)

    val fwd = runtime.expectMsgType[Api.Request]
    fwd.payload.isInstanceOf[Api.AttachVisualization] must be(true)
    val att = fwd.payload.asInstanceOf[Api.AttachVisualization]
    att.visualizationId.toString must be(visualizationId)
    att.expressionId.toString must be(nodeId)
    att.visualizationConfig.executionContextId.toString must be(contextId)
    att.visualizationConfig.expression match {
      case Api.VisualizationExpression.InFrame(expr) => expr must be(expression)
      case other                                     => fail(s"expected InFrame expression, got $other")
    }

    system.stop(actor)
  }

  it must "forward detach control messages as Api.DetachVisualization" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID().toString
    val contextId       = UUID.randomUUID().toString
    val nodeId          = UUID.randomUUID().toString
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin
    actor ! ControlMessage(attachJson)
    runtime.expectMsgType[Api.Request]

    val detachJson =
      s"""{"kind":"detach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId"}""".stripMargin
    actor ! ControlMessage(detachJson)

    val fwd = runtime.expectMsgType[Api.Request]
    fwd.payload.isInstanceOf[Api.DetachVisualization] must be(true)
    val det = fwd.payload.asInstanceOf[Api.DetachVisualization]
    det.visualizationId.toString must be(visualizationId)
    det.contextId.toString must be(contextId)
    det.expressionId.toString must be(nodeId)

    system.stop(actor)
  }

  // Regression test: after a detach the bridge must drop its correlation
  // entries (`requestState` + `requestsByVis`) so that a subsequent runtime
  // update for the same visualizationId does not get routed back as a stale
  // `ready`. Asserting via observable behaviour (no outbound messages) keeps
  // the actor's private state encapsulated.
  it must "drop correlation state after a detach so later runtime updates produce no output" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin
    actor ! ControlMessage(attachJson)
    runtime.expectMsgType[Api.Request]

    val detachJson =
      s"""{"kind":"detach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId"}""".stripMargin
    actor ! ControlMessage(detachJson)
    runtime.expectMsgType[Api.Request]

    drainAllSent(control)
    drainAllSent(data)

    // Runtime emits a late update for the now-detached visualization. With
    // the correlation cleared, `activeRequestFor` returns None and nothing
    // should land on either channel.
    system.eventStream.publish(
      Api.VisualizationUpdate(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        Array[Byte](1, 2, 3)
      )
    )

    // Give the dispatcher a moment to process the publish before asserting
    // silence on both channels.
    Thread.sleep(100)
    drainAllSent(control) must be(Nil)
    drainAllSent(data) must be(Nil)

    system.stop(actor)
  }

  // Regression test: during a client-side modify the bridge receives
  //   attach(reqB, visX); detach(reqA, visX)
  // where both share the same visualizationId. The runtime treats attach as
  // an upsert, so forwarding the detach would remove the just-replaced
  // visualization and silently strand every further update. The actor must
  // recognise the still-active rid and suppress the runtime detach.
  it must "suppress runtime detach when another rid is still attached for the same visualizationId" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val reqA            = UUID.randomUUID().toString
    val reqB            = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()

    def attachJson(reqId: String) =
      s"""{"kind":"attach","requestId":"$reqId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    def detachJson(reqId: String) =
      s"""{"kind":"detach","requestId":"$reqId",
         |"visualizationId":"$visualizationId","contextId":"$contextId"}""".stripMargin

    // Initial attach.
    actor ! ControlMessage(attachJson(reqA))
    val attachForA = runtime.expectMsgType[Api.Request]
    attachForA.payload.isInstanceOf[Api.AttachVisualization] must be(true)

    // Modify: client emits attach(newRid) before detach(oldRid) in the same
    // scan() batch. The actor forwards the second attach (runtime upserts).
    actor ! ControlMessage(attachJson(reqB))
    val attachForB = runtime.expectMsgType[Api.Request]
    attachForB.payload.isInstanceOf[Api.AttachVisualization] must be(true)

    // Detach for the old rid must NOT be forwarded as another rid is active.
    actor ! ControlMessage(detachJson(reqA))
    runtime.expectNoMessage()

    // A runtime update routes to the newest active rid (reqB), not reqA.
    system.eventStream.publish(
      Api.VisualizationUpdate(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        Array[Byte](7, 8, 9)
      )
    )
    val readyMsg = pollSentAsString(control)
    readyMsg must include(""""kind":"ready"""")
    readyMsg must include(reqB)
    readyMsg must not include reqA

    // When the remaining rid also detaches, the runtime forward does happen.
    actor ! ControlMessage(detachJson(reqB))
    val finalDetach = runtime.expectMsgType[Api.Request]
    finalDetach.payload.isInstanceOf[Api.DetachVisualization] must be(true)

    system.stop(actor)
  }

  // Regression test: when the runtime responds with an error to an attach or
  // detach we initiated, the bridge must surface it as a FailedMsg on the
  // control channel so the client slot transitions out of `pending`.
  it must "emit a failed control message on runtime error responses to attach" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    actor ! ControlMessage(attachJson)
    val fwd           = runtime.expectMsgType[Api.Request]
    val correlationId = fwd.requestId

    // Simulate the runtime rejecting the attach.
    actor ! Api.Response(
      correlationId,
      Api.ModuleNotFound("Missing.Module")
    )

    val msg = pollSentAsString(control)
    msg must include(""""kind":"failed"""")
    msg must include(requestId)
    msg must include("Missing.Module")

    system.stop(actor)
  }

  it must "emit a failed control message with diagnostic on runtime VisualizationExpressionFailed response" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    actor ! ControlMessage(attachJson)
    val fwd           = runtime.expectMsgType[Api.Request]
    val correlationId = fwd.requestId

    val diagnostic = Api.ExecutionResult.Diagnostic.error(
      message      = "boom on line 10",
      expressionId = Some(nodeId)
    )
    actor ! Api.Response(
      correlationId,
      Api.VisualizationExpressionFailed(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        "evaluation failed",
        Some(diagnostic)
      )
    )

    val msg = pollSentAsString(control)
    msg must include(""""kind":"failed"""")
    msg must include(requestId)
    msg must include("evaluation failed")
    msg must include(""""diagnostic"""")
    msg must include("boom on line 10")
    msg must include(nodeId.toString)

    system.stop(actor)
  }

  it must "forward diagnostic payload on event-stream VisualizationEvaluationFailed" in {
    val runtime = TestProbe()
    val actor = system.actorOf(
      VisualizationBridgeServer.props(runtime.ref, system.eventStream)
    )
    val control = new RecordingChannel
    val data    = new RecordingChannel
    actor ! ControlChannelEstablished(control)
    actor ! DataChannelEstablished(data)

    val requestId       = UUID.randomUUID().toString
    val visualizationId = UUID.randomUUID()
    val contextId       = UUID.randomUUID()
    val nodeId          = UUID.randomUUID()
    val attachJson =
      s"""{"kind":"attach","requestId":"$requestId",
         |"visualizationId":"$visualizationId","contextId":"$contextId",
         |"nodeExternalId":"$nodeId","request":{"visualizationModule":"M",
         |"expression":"identity"}}""".stripMargin

    actor ! ControlMessage(attachJson)
    runtime.expectMsgType[Api.Request]
    drainAllSent(control)

    val diagnostic = Api.ExecutionResult.Diagnostic.error(
      message      = "runtime panic",
      expressionId = Some(nodeId)
    )
    system.eventStream.publish(
      Api.VisualizationEvaluationFailed(
        Api.VisualizationContext(visualizationId, contextId, nodeId),
        "eval failed",
        Some(diagnostic)
      )
    )

    val msg = pollSentAsString(control)
    msg must include(""""kind":"failed"""")
    msg must include(requestId)
    msg must include(""""diagnostic"""")
    msg must include("runtime panic")

    system.stop(actor)
  }
}
