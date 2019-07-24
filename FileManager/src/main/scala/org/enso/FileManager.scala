package org.enso

import java.nio.file.Path
import java.util.UUID

import akka.actor.Scheduler
import akka.actor.typed.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.AbstractBehavior
import akka.actor.typed.scaladsl.ActorContext
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import io.methvin.watcher.DirectoryWatcher
import org.enso.filemanager.API
import org.enso.filemanager.API._

import scala.collection.mutable
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/** The main actor class.
  *
  * Implements an RPC-like protocol. Please see member types of
  * [[org.enso.filemanager.API]] for a list of supported operations and their
  * respective request-response packages.
  */
case class FileManager(projectRoot: Path, context: ActorContext[InputMessage])
    extends AbstractBehavior[API.InputMessage] {

  /** Active filesystem subtree watchers */
  val watchers: mutable.Map[UUID, DirectoryWatcher] = mutable.Map()

  def onMessageTyped[response <: Response.Success: ClassTag](
    message: Request[response]
  ): Unit = {
    val response = try {
      message.contents.validate(projectRoot)
      val result = message.contents.handle(this)
      Success(result)
    } catch { case ex: Throwable => Failure(ex) }
    context.log.debug(s"Responding with $response")
    message.replyTo ! response
  }

  override def onMessage(message: InputMessage): this.type = {
    context.log.debug(s"Received $message")
    message.handle(this)
    this
  }
}

object FileManager {
  val API: org.enso.filemanager.API.type = org.enso.filemanager.API

  /** Factory function for [[FileManager]] [[akka.actor.typed.Behavior]]. */
  def apply(projectRoot: Path): Behavior[InputMessage] =
    Behaviors.setup(context => FileManager(projectRoot, context))

  /** Convenience wrapper for
    * [[akka.actor.typed.scaladsl.AskPattern.Askable.ask]].
    *
    * It takes only the request payload (i.e. operation specific part of the
    * request) and takes care of the rest, automatically deducing the expected
    * response type.
    */
  def ask[response <: Response.Success: ClassTag](
    actor: ActorRef[API.InputMessage],
    payload: Request.Payload[response]
  )(implicit timeout: Timeout,
    scheduler: Scheduler
  ): Future[Try[response]] = {
    actor.ask { replyTo: ActorRef[Try[response]] =>
      Request(replyTo, payload)
    }
  }
}
