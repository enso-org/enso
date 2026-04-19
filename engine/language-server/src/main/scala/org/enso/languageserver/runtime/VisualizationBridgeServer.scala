package org.enso.languageserver.runtime

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.event.EventStream
import com.typesafe.scalalogging.LazyLogging
import io.circe.generic.auto._
import io.circe.parser
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.polyglot.runtime.Runtime.Api
import org.enso.ydoc.api.YjsChannel

import java.nio.ByteBuffer
import java.util.UUID
import scala.collection.mutable
import scala.util.control.NonFatal

/** Bridge between the ydoc-server visualization subdoc and the runtime.
  *
  * Receives `attach` / `detach` JSON messages on the `vis:control` channel,
  * forwards them to the runtime as `Api.AttachVisualization` /
  * `Api.DetachVisualization`, and pushes responses back as `ready` + binary
  * frame on `vis:data`, or `failed` JSON on `vis:control`. One-shot
  * evaluations are just attach requests whose expression is 
  * `Api.VisualizationExpression.InFrame`. The runtime auto-detaches them 
  * after one update, and the bridge cleans up the correlation maps eagerly 
  * on first response.
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
      channel.subscribe { (msg: Object) =>
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

  final case class ReadyMsg(requestId: RequestId) extends ControlMsg

  final case class FailedMsg(
    requestId: RequestId,
    message: String
  ) extends ControlMsg

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

  /** Decoders for the control-channel message envelope. */
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

    implicit val readyDecoder: Decoder[ReadyMsg] =
      Decoder.forProduct1("requestId")(ReadyMsg.apply)

    implicit val failedDecoder: Decoder[FailedMsg] =
      Decoder.forProduct2("requestId", "message")(FailedMsg.apply)

    implicit val controlDecoder: Decoder[ControlMsg] = Decoder.instance {
      cursor =>
        cursor.downField("kind").as[String].flatMap {
          case "attach" => attachDecoder.tryDecode(cursor): Result[ControlMsg]
          case "detach" => detachDecoder.tryDecode(cursor): Result[ControlMsg]
          case "ready"  => readyDecoder.tryDecode(cursor): Result[ControlMsg]
          case "failed" => failedDecoder.tryDecode(cursor): Result[ControlMsg]
          case other =>
            Left(
              io.circe.DecodingFailure(
                s"Unknown vis:control message kind: $other",
                cursor.history
              )
            )
        }
    }

    implicit val readyEncoder: Encoder[ReadyMsg] = Encoder.instance { m =>
      Json.obj(
        "kind"      -> Json.fromString("ready"),
        "requestId" -> Json.fromString(m.requestId)
      )
    }

    implicit val failedEncoder: Encoder[FailedMsg] = Encoder.instance { m =>
      Json.obj(
        "kind"      -> Json.fromString("failed"),
        "requestId" -> Json.fromString(m.requestId),
        "message"   -> Json.fromString(m.message)
      )
    }
  }

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

  /** visualizationId (string UUID) -> in-flight requestId (string UUID).
    *
    * Used to correlate runtime `VisualizationUpdate` events back to the
    * originating vis slot. Cleared on detach.
    */
  private val visToRequest: mutable.Map[String, String] = mutable.Map.empty

  /** Tracks the contextId for each visualizationId so we can honor detaches
    * from the runtime without re-parsing messages.
    */
  private val visToContext: mutable.Map[String, UUID] = mutable.Map.empty

  /** Tracks the expressionId (node external id) for each visualizationId for
    * detach messaging.
    */
  private val visToExpression: mutable.Map[String, UUID] = mutable.Map.empty

  /** Visualization ids that were attached with an `InFrame` expression.
    * These are terminal on first response, the runtime auto-detaches the
    * underlying oneshot via `VisualizationHolder.getOneshotExpression.remove()`,
    * so we drop our correlation entries eagerly instead of waiting for a
    * client detach that will never arrive.
    */
  private val oneshotVisIds: mutable.Set[String] = mutable.Set.empty

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

    case Api.VisualizationEvaluationFailed(ctx, message, _) =>
      handleEvaluationFailed(ctx, message)
  }

  private def handleControl(json: String): Unit = {
    import Codecs._
    parser.decode[ControlMsg](json) match {
      case Right(msg: AttachMsg) => forwardAttach(msg)
      case Right(msg: DetachMsg) => forwardDetach(msg)
      case Right(
            _: ReadyMsg
          ) => // Ignore our own echo; filtering is best-effort
      case Right(_: FailedMsg) => // Ditto
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
    val e       = msg.request.expression
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

    visToRequest.put(msg.visualizationId, msg.requestId)
    visToContext.put(msg.visualizationId, contextId)
    visToExpression.put(msg.visualizationId, expressionId)
    if (isInFrame) oneshotVisIds.add(msg.visualizationId)

    val apiReq = Api.AttachVisualization(
      visualizationId     = visualizationId,
      expressionId        = expressionId,
      visualizationConfig = config.toApi
    )
    runtime ! Api.Request(UUID.randomUUID(), apiReq)
  }

  private def forwardDetach(msg: DetachMsg): Unit = {
    val visualizationId = parseUuidOr(msg.visualizationId).getOrElse {
      // Best-effort: a slot was removed before we learned its fields. Ignore.
      visToRequest.remove(msg.visualizationId)
      return
    }
    val contextId    = visToContext.getOrElse(msg.visualizationId, null)
    val expressionId = visToExpression.getOrElse(msg.visualizationId, null)
    visToRequest.remove(msg.visualizationId)
    visToContext.remove(msg.visualizationId)
    visToExpression.remove(msg.visualizationId)
    oneshotVisIds.remove(msg.visualizationId)
    if (contextId == null || expressionId == null) {
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
    runtime ! Api.Request(UUID.randomUUID(), apiReq)
  }

  private def handleVisualizationUpdate(
    ctx: Api.VisualizationContext,
    bytes: Array[Byte]
  ): Unit = {
    val visIdStr = ctx.visualizationId.toString
    val reqIdOpt = visToRequest.get(visIdStr)
    reqIdOpt match {
      case None =>
        // Not a visualization we are tracking.
        logger.debug(
          s"vis: runtime emitted VisualizationUpdate for untracked $visIdStr"
        )
      case Some(requestId) =>
        sendDataFrame(requestId, bytes)
        sendControl(ReadyMsg(requestId))(Codecs.readyEncoder)
        // InFrame oneshots are terminal. The runtime will not emit further
        // updates for this visualization id, so drop our correlation state.
        if (oneshotVisIds.remove(visIdStr)) {
          visToRequest.remove(visIdStr)
          visToContext.remove(visIdStr)
          visToExpression.remove(visIdStr)
        }
    }
  }

  private def handleEvaluationFailed(
    ctx: Api.VisualizationContext,
    message: String
  ): Unit = {
    val visIdStr = ctx.visualizationId.toString
    visToRequest.get(visIdStr) match {
      case None => ()
      case Some(requestId) =>
        sendControl(FailedMsg(requestId, message))(Codecs.failedEncoder)
        if (oneshotVisIds.remove(visIdStr)) {
          visToRequest.remove(visIdStr)
          visToContext.remove(visIdStr)
          visToExpression.remove(visIdStr)
        }
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

  private def sendControl[T <: ControlMsg](msg: T)(implicit
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
