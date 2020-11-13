package org.enso.languageserver.data
import java.util.UUID

import io.circe._
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.runtime.ExecutionApi.ContextId

/**
  * A superclass for all capabilities in the system.
  * @param method method name used to identify the capability.
  */
sealed abstract class Capability(val method: String)

//TODO[MK]: Migrate to actual Path, once it is implemented.
/**
  * A capability allowing the user to modify a given file.
  * @param path the file path this capability is granted for.
  */
case class CanEdit(path: Path) extends Capability(CanEdit.methodName)

object CanEdit {
  val methodName = "text/canEdit"
}

/**
  * A capability allowing user to receive file events.
  *
  * @param path path to watch.
  */
case class ReceivesTreeUpdates(path: Path)
    extends Capability(ReceivesTreeUpdates.methodName)

object ReceivesTreeUpdates {
  val methodName = "file/receivesTreeUpdates"
}

/**
  * A capability allowing user to modify the execution context.
  *
  * @param contextId identifier of an execution conatext
  */
case class CanModify(contextId: ContextId)
    extends Capability(CanModify.methodName)

object CanModify {
  val methodName = "executionContext/canModify"
}

/**
  * A capability allowing user to receive events from the execution context.
  *
  * @param contextId identifier of an execution conatext
  */
case class ReceivesUpdates(contextId: ContextId)
    extends Capability(ReceivesUpdates.methodName)

object ReceivesUpdates {
  val methodName = "executionContext/receivesUpdates"
}

object Capability {
  import io.circe.generic.auto._
  import io.circe.syntax._

  implicit val encoder: Encoder[Capability] = {
    case cap: CanEdit                            => cap.asJson
    case cap: ReceivesTreeUpdates                => cap.asJson
    case cap: CanModify                          => cap.asJson
    case cap: ReceivesUpdates                    => cap.asJson
    case cap: ReceivesSuggestionsDatabaseUpdates => cap.asJson
  }

}

case class ReceivesSuggestionsDatabaseUpdates()
    extends Capability(ReceivesSuggestionsDatabaseUpdates.methodName)

object ReceivesSuggestionsDatabaseUpdates {
  val methodName = "search/receivesSuggestionsDatabaseUpdates"
}

/**
  * A capability registration object, used to identify acquired capabilities.
  *
  * @param capability the registered capability.
  */
case class CapabilityRegistration(
  capability: Capability
)

object CapabilityRegistration {
  import io.circe.generic.auto._
  import io.circe.syntax._

  type Id = UUID

  private val methodField  = "method"
  private val optionsField = "registerOptions"

  implicit val encoder: Encoder[CapabilityRegistration] = registration =>
    Json.obj(
      methodField  -> registration.capability.method.asJson,
      optionsField -> registration.capability.asJson
    )

  implicit val decoder: Decoder[CapabilityRegistration] = json => {
    def resolveOptions(
      method: String,
      json: Json
    ): Decoder.Result[Capability] =
      method match {
        case CanEdit.methodName             => json.as[CanEdit]
        case ReceivesTreeUpdates.methodName => json.as[ReceivesTreeUpdates]
        case CanModify.methodName           => json.as[CanModify]
        case ReceivesUpdates.methodName     => json.as[ReceivesUpdates]
        case ReceivesSuggestionsDatabaseUpdates.methodName =>
          json.as[ReceivesSuggestionsDatabaseUpdates]
        case _ =>
          Left(DecodingFailure("Unrecognized capability method.", List()))
      }

    for {
      method <- json.downField(methodField).as[String]
      capability <- resolveOptions(
        method,
        json.downField(optionsField).focus.getOrElse(Json.Null)
      )
    } yield CapabilityRegistration(capability)
  }
}
