package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.EventStream
import com.typesafe.scalalogging.LazyLogging
import io.circe.parser
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.ydoc.api.YjsChannel

import java.nio.ByteBuffer
import java.util.UUID
import java.util.function.Consumer
import scala.collection.mutable
import scala.util.control.NonFatal

/** Bridge between the ydoc-server visualization subdoc and the runtime.
  *
  * Receives `attach` / `detach` JSON messages on the `vis:control` channel,
  * forwards them to the runtime as `Api.AttachVisualization` /
  * `Api.DetachVisualization`, and pushes responses back as `ready` + binary
  * frame on `vis:data`, or `failed` JSON on `vis:control`. One-shot
  * evaluations are attach requests whose expression is
  * `Api.VisualizationExpression.InFrame`. The runtime auto-detaches them
  * after one update and the bridge drops its correlation eagerly.
  *
  * Correlation state is keyed by **bridge requestId**, not by
  * `visualizationId`. A modify is expressed by the client as
  * `removeSlot(oldRid); createSlot(newRid)` with the **same** visualizationId,
  * and `scan()` in `visualizationBridge.ts` emits `attach(newRid)` before
  * `detach(oldRid)`. If we keyed correlation by visualizationId, the detach
  * would wipe the just-installed entry for the new rid and silently orphan
  * every subsequent runtime update. Here we track all in-flight rids per
  * visualizationId in an ordered set and forward the detach to the runtime
  * only when it would leave no other tracked rid for that visualizationId
  * (because the runtime treats `Api.AttachVisualization` as an upsert. A
  * second attach for the same id replaces the first, so emitting a detach
  * after a second attach would remove the newer upsert too).
  *
  * There is a single bridge actor per Language Server process. Both callback
  * classes (control + data) feed channel references into the same actor.
  */
object VisualizationBridgeServer {

  /** Inbound message: a control channel has been established. */
  final case class ControlChannelEstablished(channel: YjsChannel)

  /** Inbound message: a data channel has been established. */
  final case class DataChannelEstablished(channel: YjsChannel)

  /** Inbound message: raw JSON string received on the control channel. */
  final case class ControlMessage(json: String)

  /** Request id (UUID serialized as a string). */
  type RequestId = String

  /** YjsChannel.Server for the `vis:control` (JSON) channel. */
  final class ControlServerCallbacks(bridge: ActorRef)
      extends YjsChannel.Server
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.trace(s"vis:control channel connected")
      bridge ! ControlChannelEstablished(channel)
      channel.subscribe(new ControlMessageHandler(bridge))
    }
  }

  /** Receives raw inbound messages on the `vis:control` channel and forwards
    * decoded `String` payloads to the bridge actor. Defined as a named class
    * (rather than a Scala lambda) so that GraalVM Native Image can include
    * its `accept` method via a single explicit reflection-config entry.
    */
  final class ControlMessageHandler(bridge: ActorRef)
      extends Consumer[Object]
      with LazyLogging {

    override def accept(msg: Object): Unit = {
      try {
        val text = Option(msg).collect {
          case s: String       => s
          case c: CharSequence => c.toString
        }.orNull
        if (text != null) bridge ! ControlMessage(text)
        else logger.warn(s"vis:control non-string message: ${msg.getClass}")
      } catch {
        case NonFatal(e) =>
          logger.error("Error handling vis:control message", e)
      }
    }
  }

  /** YjsChannel.Server for the `vis:data` (binary) channel. The bridge does
    * not subscribe here. Responses flow LS -> ydoc. We only record the
    * channel so the actor can emit binary frames.
    */
  final class DataServerCallbacks(bridge: ActorRef)
      extends YjsChannel.Server
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.trace(s"vis:data channel connected")
      bridge ! DataChannelEstablished(channel)
    }
  }

  /** Control-channel message envelope. */
  sealed trait ControlMsg {
    def requestId: RequestId
  }

  final case class AttachMsg(
    requestId: RequestId,
    visualizationId: String,
    contextId: String,
    nodeExternalId: String,
    request: VisRequestPayload
  ) extends ControlMsg

  final case class DetachMsg(
    requestId: RequestId,
    visualizationId: String,
    contextId: String
  ) extends ControlMsg

  final case class FailedMsg(
    requestId: RequestId,
    message: String,
    diagnostic: Option[DiagnosticPayload]
  ) extends ControlMsg

  /** Diagnostic payload shape matching the client's `Diagnostic` type in
    * `ydoc-shared/src/languageServerTypes.ts`. Only fields that survive the
    * JSON bridge are populated; `path` is omitted because the client expects a
    * content-root-relative path and the runtime only carries a `File`.
    */
  final case class DiagnosticPayload(
    kind: String,
    message: String,
    location: Option[Json],
    expressionId: Option[String],
    stack: Vector[DiagnosticStackElement]
  )

  final case class DiagnosticStackElement(
    functionName: String,
    expressionId: Option[String]
  )

  /** The immutable preprocessor portion of a visualization request. Matches
    * the client-side `VisRequestPreprocessor` type in `ydoc-shared`.
    */
  final case class VisRequestPayload(
    visualizationModule: String,
    expression: VisExpression,
    positionalArgumentsExpressions: Option[Vector[String]]
  )

  /** Union wrapper for `expression: string | MethodPointer | { inFrame }` in
    * the request. Exactly one field is populated. InFrame carries its payload
    * as a nested `{ "inFrame": "..." }` object to keep the string-shorthand
    * decoding of plain text expressions unambiguous.
    */
  final case class VisExpression(
    text: Option[String],
    methodPointer: Option[MethodPointer],
    inFrame: Option[String]
  )

  implicit val methodPointerDecoder: Decoder[MethodPointer] =
    Decoder.forProduct3("module", "definedOnType", "name")(MethodPointer.apply)

  object VisExpression {

    /** Custom decoder that accepts:
      *   - a bare JSON string -> `Text`
      *   - an object matching `MethodPointer` -> `ModuleMethod`
      *   - `{ "inFrame": "..." }` -> `InFrame`
      */
    implicit val decoder: Decoder[VisExpression] = Decoder.instance { cursor =>
      cursor.as[String] match {
        case Right(s) => Right(VisExpression(Some(s), None, None))
        case Left(_) =>
          cursor.downField("inFrame").as[String] match {
            case Right(expr) => Right(VisExpression(None, None, Some(expr)))
            case Left(_) =>
              cursor.as[MethodPointer].map { mp =>
                VisExpression(None, Some(mp), None)
              }
          }
      }
    }
  }

  /** Decoders for the control-channel message envelope. The LS only ever
    * decodes the `attach` / `detach` kinds; `ready` and `failed` are LS-side
    * outbound and echoes are already filtered by `YjsChannel`'s senderId
    * guard, so they are intentionally not decoded here.
    */
  private[runtime] object Codecs {
    import io.circe.Decoder.Result

    implicit val visRequestDecoder: Decoder[VisRequestPayload] =
      Decoder.forProduct3(
        "visualizationModule",
        "expression",
        "positionalArgumentsExpressions"
      )(VisRequestPayload.apply)

    implicit val attachDecoder: Decoder[AttachMsg] =
      Decoder.forProduct5(
        "requestId",
        "visualizationId",
        "contextId",
        "nodeExternalId",
        "request"
      )(AttachMsg.apply)

    implicit val detachDecoder: Decoder[DetachMsg] =
      Decoder.forProduct3(
        "requestId",
        "visualizationId",
        "contextId"
      )(DetachMsg.apply)

    implicit val controlDecoder: Decoder[ControlMsg] = Decoder.instance {
      cursor =>
        cursor.downField("kind").as[String].flatMap {
          case "attach" => attachDecoder.tryDecode(cursor): Result[ControlMsg]
          case "detach" => detachDecoder.tryDecode(cursor): Result[ControlMsg]
          case other =>
            Left(
              io.circe.DecodingFailure(
                s"Unknown vis:control message kind: $other",
                cursor.history
              )
            )
        }
    }

    implicit val readyEncoder: Encoder[ReadyMsgOut] = Encoder.instance { m =>
      Json.obj(
        "kind"      -> Json.fromString("ready"),
        "requestId" -> Json.fromString(m.requestId)
      )
    }

    implicit val diagnosticStackElementEncoder
      : Encoder[DiagnosticStackElement] =
      Encoder.instance { s =>
        val base = Json.obj(
          "functionName" -> Json.fromString(s.functionName)
        )
        s.expressionId match {
          case Some(id) =>
            base.deepMerge(Json.obj("expressionId" -> Json.fromString(id)))
          case None => base
        }
      }

    implicit val diagnosticPayloadEncoder: Encoder[DiagnosticPayload] =
      Encoder.instance { d =>
        val fields = scala.collection.mutable.ArrayBuffer.empty[(String, Json)]
        fields += ("kind"    -> Json.fromString(d.kind))
        fields += ("message" -> Json.fromString(d.message))
        d.location.foreach(loc => fields += ("location" -> loc))
        d.expressionId.foreach(id =>
          fields += ("expressionId" -> Json.fromString(id))
        )
        fields += ("stack" -> Json.fromValues(d.stack.map(_.asJson)))
        Json.obj(fields.toSeq: _*)
      }

    implicit val failedEncoder: Encoder[FailedMsg] = Encoder.instance { m =>
      val base = Json.obj(
        "kind"      -> Json.fromString("failed"),
        "requestId" -> Json.fromString(m.requestId),
        "message"   -> Json.fromString(m.message)
      )
      m.diagnostic match {
        case Some(d) =>
          base.deepMerge(Json.obj("diagnostic" -> d.asJson))
        case None => base
      }
    }
  }

  /** Outbound-only marker for the `ready` control message. Kept as a distinct
    * type from `ControlMsg` because the LS never decodes it.
    */
  final case class ReadyMsgOut(requestId: RequestId)

  /** Per-request correlation info. Keyed by bridge requestId so that two
    * slots sharing a visualizationId (a modify is exactly that: attach(new)
    * + detach(old) with the same visualizationId) do not clobber each
    * other's entry.
    */
  final case class RequestInfo(
    visualizationId: String,
    contextId: UUID,
    expressionId: UUID,
    isInFrame: Boolean
  )

  /** Create the bridge actor. */
  def props(runtime: ActorRef, eventStream: EventStream): Props =
    Props(new VisualizationBridgeActor(runtime, eventStream))

  /** Build the two `YjsChannel.Server` callback classes bound to the given
    * bridge actor. Returned in the order (control, data).
    */
  def callbacks(
    bridge: ActorRef
  ): (YjsChannel.Server, YjsChannel.Server) =
    (new ControlServerCallbacks(bridge), new DataServerCallbacks(bridge))

  /** Convert a runtime diagnostic to the JSON-serializable shape the client
    * consumes. Kept small: the client's `Diagnostic` also has `path`, but we
    * drop it here because mapping `File` to a content-root-relative path
    * requires a `ContentRootManager`, and the bridge actor doesn't have one.
    * If path-aware diagnostics become important the conversion can grow.
    */
  private[runtime] def toDiagnosticPayload(
    diagnostic: Api.ExecutionResult.Diagnostic
  ): DiagnosticPayload = {
    val kindStr = diagnostic.kind match {
      case Api.DiagnosticType.Error   => "Error"
      case Api.DiagnosticType.Warning => "Warning"
    }
    val stack = diagnostic.stack.map { element =>
      DiagnosticStackElement(
        functionName = element.functionName,
        expressionId = element.expressionId.map(_.toString)
      )
    }
    DiagnosticPayload(
      kind         = kindStr,
      message      = diagnostic.message.getOrElse(""),
      location     = None,
      expressionId = diagnostic.expressionId.map(_.toString),
      stack        = stack
    )
  }
}

/** Stateful actor that mediates between vis channels and the runtime. */
final class VisualizationBridgeActor(
  runtime: ActorRef,
  eventStream: EventStream
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import VisualizationBridgeServer._

  private var controlChannel: Option[YjsChannel] = None
  private var dataChannel: Option[YjsChannel]    = None

  /** Per-request correlation table. Populated on attach, cleared on detach
    * (client-driven) or on terminal InFrame response (eager cleanup).
    */
  private val requestState: mutable.Map[RequestId, RequestInfo] =
    mutable.Map.empty

  /** For each visualizationId, the ordered set of in-flight bridge request
    * ids. Insertion order is preserved so that "most recent" can be read off
    * the tail when routing runtime updates. During a modify, this set
    * transiently contains two entries (old rid + new rid) until the detach
    * arrives.
    */
  private val requestsByVis
    : mutable.Map[String, mutable.LinkedHashSet[RequestId]] =
    mutable.Map.empty

  /** UUID of `Api.Request` sent to runtime -> bridge requestId that triggered
    * it. Used so that error `Api.Response`s correlated back by the runtime
    * can be translated into `FailedMsg` on the client's control channel.
    */
  private val runtimeToRequest: mutable.Map[UUID, RequestId] =
    mutable.Map.empty

  override def preStart(): Unit = {
    // `RuntimeConnector` publishes the inner notification payload directly on
    // the event stream (see `RuntimeConnector.scala`), not the wrapping
    // `Api.Response`.  Subscribe to the concrete types.
    eventStream.subscribe(self, classOf[Api.VisualizationUpdate])
    eventStream.subscribe(self, classOf[Api.VisualizationEvaluationFailed])
  }

  override def postStop(): Unit = {
    eventStream.unsubscribe(self)
  }

  override def receive: Receive = {
    case ControlChannelEstablished(ch) =>
      logger.trace("vis bridge received control channel")
      controlChannel = Some(ch)

    case DataChannelEstablished(ch) =>
      logger.trace("vis bridge received data channel")
      dataChannel = Some(ch)

    case ControlMessage(json) =>
      handleControl(json)

    case Api.VisualizationUpdate(ctx, bytes) =>
      handleVisualizationUpdate(ctx, bytes)

    case Api.VisualizationEvaluationFailed(ctx, message, diagnostic) =>
      handleEvaluationFailed(ctx, message, diagnostic)

    case Api.Response(Some(correlationId), payload) =>
      handleRuntimeResponse(correlationId, payload)
  }

  private def handleControl(json: String): Unit = {
    import Codecs._
    parser.decode[ControlMsg](json) match {
      case Right(msg: AttachMsg) => forwardAttach(msg)
      case Right(msg: DetachMsg) => forwardDetach(msg)
      case Right(_)              => // Unreachable: decoder only produces attach/detach.
      case Left(err) =>
        logger.warn(s"vis:control failed to decode message: $err")
    }
  }

  private def forwardAttach(msg: AttachMsg): Unit = {
    val visualizationId = parseUuidOr(msg.visualizationId) match {
      case Some(id) => id
      case None =>
        logger.warn(
          s"vis attach: invalid visualizationId ${msg.visualizationId}"
        )
        return
    }
    val contextId = parseUuidOr(msg.contextId) match {
      case Some(id) => id
      case None =>
        logger.warn(s"vis attach: invalid contextId ${msg.contextId}")
        return
    }
    val expressionId = parseUuidOr(msg.nodeExternalId) match {
      case Some(id) => id
      case None =>
        logger.warn(s"vis attach: invalid nodeExternalId ${msg.nodeExternalId}")
        return
    }

    val args =
      msg.request.positionalArgumentsExpressions.getOrElse(Vector.empty)
    val e         = msg.request.expression
    val isInFrame = e.inFrame.isDefined
    val visExpr = (e.text, e.methodPointer, e.inFrame) match {
      case (Some(text), _, _) =>
        VisualizationExpression.Text(
          msg.request.visualizationModule,
          text,
          args
        )
      case (_, Some(mp), _) =>
        VisualizationExpression.ModuleMethod(mp, args)
      case (_, _, Some(expr)) =>
        VisualizationExpression.InFrame(expr)
      case _ =>
        logger.warn(
          "vis attach: request.expression missing text/methodPointer/inFrame"
        )
        return
    }

    val config = VisualizationConfiguration(
      executionContextId  = contextId,
      expression          = visExpr,
      visualizationModule = msg.request.visualizationModule
    )

    requestState.put(
      msg.requestId,
      RequestInfo(
        visualizationId = msg.visualizationId,
        contextId       = contextId,
        expressionId    = expressionId,
        isInFrame       = isInFrame
      )
    )
    requestsByVis
      .getOrElseUpdate(msg.visualizationId, mutable.LinkedHashSet.empty)
      .add(msg.requestId)

    val apiReq = Api.AttachVisualization(
      visualizationId     = visualizationId,
      expressionId        = expressionId,
      visualizationConfig = config.toApi
    )
    val runtimeReqId = UUID.randomUUID()
    runtimeToRequest.put(runtimeReqId, msg.requestId)
    runtime ! Api.Request(runtimeReqId, apiReq)
  }

  private def forwardDetach(msg: DetachMsg): Unit = {
    val infoOpt  = requestState.remove(msg.requestId)
    val visIdStr = infoOpt.map(_.visualizationId).getOrElse(msg.visualizationId)
    // Remove this rid from the ordered set; drop the set entry entirely when
    // it becomes empty.
    requestsByVis.get(visIdStr).foreach { set =>
      set.remove(msg.requestId)
      if (set.isEmpty) requestsByVis.remove(visIdStr)
    }

    val stillActiveForVis = requestsByVis.contains(visIdStr)
    if (stillActiveForVis) {
      // A newer attach for the same visualizationId is still in flight. The
      // runtime performs an upsert on attach, so the earlier attach has
      // already been replaced. Forwarding a detach now would remove the
      // newer entry too, silently stranding its updates.
      logger.trace(
        s"vis detach: suppressing runtime forward for visualizationId=$visIdStr; " +
        s"another in-flight request is still active"
      )
      return
    }

    val visualizationId = parseUuidOr(visIdStr).getOrElse {
      logger.warn(
        s"vis detach: invalid or missing visualizationId '$visIdStr'; " +
        s"cannot forward to runtime for requestId ${msg.requestId}"
      )
      return
    }
    val (contextId, expressionId) = infoOpt match {
      case Some(info) => (info.contextId, info.expressionId)
      case None =>
        logger.warn(
          s"vis detach: missing tracked context/expression for $visualizationId"
        )
        return
    }
    val apiReq = Api.DetachVisualization(
      contextId       = contextId,
      visualizationId = visualizationId,
      expressionId    = expressionId
    )
    val runtimeReqId = UUID.randomUUID()
    runtimeToRequest.put(runtimeReqId, msg.requestId)
    runtime ! Api.Request(runtimeReqId, apiReq)
  }

  private def handleVisualizationUpdate(
    ctx: Api.VisualizationContext,
    bytes: Array[Byte]
  ): Unit = {
    val visIdStr = ctx.visualizationId.toString
    activeRequestFor(visIdStr) match {
      case None =>
        logger.debug(
          s"vis: runtime emitted VisualizationUpdate for untracked $visIdStr"
        )
      case Some(requestId) =>
        sendDataFrame(requestId, bytes)
        sendControl(ReadyMsgOut(requestId))(Codecs.readyEncoder)
        // InFrame oneshots are terminal. The runtime will not emit further
        // updates for this visualization id, so drop our correlation state.
        if (requestState.get(requestId).exists(_.isInFrame)) {
          dropRequest(requestId, visIdStr)
        }
    }
  }

  private def handleEvaluationFailed(
    ctx: Api.VisualizationContext,
    message: String,
    diagnostic: Option[Api.ExecutionResult.Diagnostic]
  ): Unit = {
    val visIdStr = ctx.visualizationId.toString
    activeRequestFor(visIdStr) match {
      case None => ()
      case Some(requestId) =>
        sendControl(
          FailedMsg(
            requestId,
            message,
            diagnostic.map(toDiagnosticPayload)
          )
        )(Codecs.failedEncoder)
        if (requestState.get(requestId).exists(_.isInFrame)) {
          dropRequest(requestId, visIdStr)
        }
    }
  }

  /** Route a runtime response back to the bridge requestId that produced the
    * original `Api.Request`. Success responses (`VisualizationAttached`,
    * `VisualizationDetached`) are acknowledged silently; error payloads are
    * surfaced to the client as a `FailedMsg` so the slot transitions out of
    * `pending`.
    */
  private def handleRuntimeResponse(
    correlationId: UUID,
    payload: Runtime.ApiResponse
  ): Unit = {
    val bridgeReqId = runtimeToRequest.remove(correlationId) match {
      case Some(id) => id
      case None     =>
        // Not a response to a request this bridge initiated.
        return
    }
    payload match {
      case _: Api.VisualizationAttached => ()
      case _: Api.VisualizationDetached => ()
      case Api.VisualizationExpressionFailed(_, message, failure) =>
        emitFailure(bridgeReqId, message, failure)
      case _: Api.VisualizationNotFound =>
        emitFailure(bridgeReqId, "Visualization not found", None)
      case Api.ModuleNotFound(moduleName) =>
        emitFailure(bridgeReqId, s"Module not found: $moduleName", None)
      case Api.ContextNotExistError(contextId) =>
        emitFailure(
          bridgeReqId,
          s"Execution context does not exist: $contextId",
          None
        )
      case other =>
        logger.debug(
          s"vis: ignoring runtime response for $bridgeReqId: $other"
        )
    }
  }

  private def emitFailure(
    bridgeReqId: RequestId,
    message: String,
    diagnostic: Option[Api.ExecutionResult.Diagnostic]
  ): Unit = {
    // Always clean up: the runtime won't emit further events for this rid.
    val visIdOpt = requestState.get(bridgeReqId).map(_.visualizationId)
    visIdOpt.foreach(visIdStr => dropRequest(bridgeReqId, visIdStr))
    sendControl(
      FailedMsg(bridgeReqId, message, diagnostic.map(toDiagnosticPayload))
    )(Codecs.failedEncoder)
  }

  /** Pick the most recent in-flight bridge requestId for a visualizationId.
    * Because `mutable.LinkedHashSet` preserves insertion order, the tail of
    * the set is the latest attach.
    */
  private def activeRequestFor(visIdStr: String): Option[RequestId] =
    requestsByVis.get(visIdStr).flatMap { set =>
      if (set.isEmpty) None else Some(set.last)
    }

  private def dropRequest(requestId: RequestId, visIdStr: String): Unit = {
    requestState.remove(requestId)
    requestsByVis.get(visIdStr).foreach { set =>
      set.remove(requestId)
      if (set.isEmpty) requestsByVis.remove(visIdStr)
    }
  }

  private def sendDataFrame(requestId: String, payload: Array[Byte]): Unit = {
    val uuid = parseUuidOr(requestId).getOrElse {
      logger.warn(s"vis: cannot frame data, invalid requestId $requestId")
      return
    }
    dataChannel match {
      case None => logger.warn("vis: data channel not ready; dropping frame")
      case Some(ch) =>
        val buffer = ByteBuffer.allocateDirect(16 + payload.length)
        buffer.putLong(uuid.getMostSignificantBits)
        buffer.putLong(uuid.getLeastSignificantBits)
        buffer.put(payload)
        buffer.flip()
        ch.send(buffer)
    }
  }

  private def sendControl[T](msg: T)(implicit
    enc: Encoder[T]
  ): Unit = {
    controlChannel match {
      case None =>
        logger.warn("vis: control channel not ready; dropping message")
      case Some(ch) => ch.send(msg.asJson.noSpaces)
    }
  }

  private def parseUuidOr(s: String): Option[UUID] =
    try Some(UUID.fromString(s))
    catch { case _: IllegalArgumentException => None }
}

object VisualizationBridgeActor {

  /** Factory for building the bridge actor and its two callback classes as a
    * bundle, given an actor system + runtime + event stream. The caller is
    * responsible for passing the callbacks to `YdocServerApi.launchYdocServer`
    * and for managing the returned `ActorRef`'s lifecycle.
    */
  def apply(
    runtime: ActorRef,
    system: ActorSystem
  ): VisualizationBridgeBundle = {
    val ref = system.actorOf(
      VisualizationBridgeServer.props(runtime, system.eventStream),
      name = s"visualization-bridge-${UUID.randomUUID()}"
    )
    val (control, data) = VisualizationBridgeServer.callbacks(ref)
    VisualizationBridgeBundle(ref, control, data)
  }
}

final case class VisualizationBridgeBundle(
  actor: ActorRef,
  controlCallbacks: YjsChannel.Server,
  dataCallbacks: YjsChannel.Server
)
