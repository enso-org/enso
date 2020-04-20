package org.enso.languageserver.text

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityAcquisitionBadRequest,
  CapabilityReleaseBadRequest,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CanEdit,
  CapabilityRegistration,
  ContentBasedVersioning
}
import org.enso.languageserver.filemanager.Path
import org.enso.languageserver.monitoring.MonitoringProtocol.{Ping, Pong}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.text.TextProtocol.{
  ApplyEdit,
  CloseFile,
  FileNotOpened,
  OpenFile,
  SaveFile
}

/**
  * An actor that routes request regarding text editing to the right buffer.
  * It creates a buffer actor, if a buffer doesn't exists.
  *
  * @param fileManager a file manager
  * @param versionCalculator a content based version calculator
  */
class BufferRegistry(fileManager: ActorRef)(
  implicit versionCalculator: ContentBasedVersioning
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = running(Map.empty)

  private def running(registry: Map[Path, ActorRef]): Receive = {
    case Ping =>
      sender() ! Pong

    case msg @ OpenFile(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        val bufferRef =
          context.actorOf(CollaborativeBuffer.props(path, fileManager))
        context.watch(bufferRef)
        bufferRef.forward(msg)
        context.become(running(registry + (path -> bufferRef)))
      }

    case Terminated(bufferRef) =>
      context.become(running(registry.filter(_._2 != bufferRef)))

    case msg @ CloseFile(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ AcquireCapability(_, CapabilityRegistration(CanEdit(path))) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! CapabilityAcquisitionBadRequest
      }

    case msg @ ReleaseCapability(_, CapabilityRegistration(CanEdit(path))) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! CapabilityReleaseBadRequest
      }

    case msg @ ApplyEdit(_, FileEdit(path, _, _, _)) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }

    case msg @ SaveFile(_, path, _) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        sender() ! FileNotOpened
      }
  }

}

object BufferRegistry {

  /**
    * Creates a configuration object used to create a [[BufferRegistry]]
    *
    * @param fileManager a file manager actor
    * @param versionCalculator a content based version calculator
    * @return a configuration object
    */
  def props(
    fileManager: ActorRef
  )(implicit versionCalculator: ContentBasedVersioning): Props =
    Props(new BufferRegistry(fileManager))

}
