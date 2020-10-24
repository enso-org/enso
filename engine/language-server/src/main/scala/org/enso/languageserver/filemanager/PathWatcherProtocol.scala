package org.enso.languageserver.filemanager

import akka.actor.ActorRef

object PathWatcherProtocol {

  /** Requests event manager to watch the path.
    *
    * @param path path to watch
    */
  case class WatchPath(path: Path, clients: Set[ActorRef])

  case object WatchPath {

    def apply(path: Path, client: ActorRef): WatchPath =
      WatchPath(path, Set(client))
  }

  /** Requests event manager to stop watching.
    */
  case class UnwatchPath(client: ActorRef)

  /** Returns a file event result.
    *
    * @param result file event
    */
  case class FileEventResult(result: FileEvent)

}
