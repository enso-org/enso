package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import org.enso.languageserver.capability.CapabilityProtocol.{
  AcquireCapability,
  CapabilityNotAcquiredResponse,
  ReleaseCapability
}
import org.enso.languageserver.data.{
  CapabilityRegistration,
  Config,
  ReceivesTreeUpdates
}
import org.enso.languageserver.effect._
import org.enso.languageserver.util.UnhandledLogging

/** Handles `receivesTreeUpdates` capabilities acquisition and release.
  *
  * == Implementation ==
  *
  * Legend:
  *
  *   - 1  - Singleton
  *   - *C - Created per client
  *   - *P - Created for each watched Path
  *   - *H - Request is forwarded to intermediate handler. Created per request.
  *
  * {{{
  *                   *C                                      1
  *  +------------------+   *H    +----------------------------+
  *  | ClientController +-------->+ ReceivesTreeUpdatesHandler |
  *  +--------------+---+         +---------+------------------+
  *                 ^                       |
  *                 |                       |
  *                 |                       v      *P
  *                 |             +---------+--------+
  *                 +-------------+   PathWatcher    |
  *                               +---------+--------+
  *                                         ^
  *                                         |
  *                                         |      *P
  *                               +---------+--------+
  *                               |  WatcherAdapter  |
  *                               +------------------+
  *
  * }}}
  *
  * @param config configuration
  * @param fs file system
  * @param exec executor of file system events
  */
final class ReceivesTreeUpdatesHandler(
  config: Config,
  fs: FileSystemApi[BlockingIO],
  exec: Exec[BlockingIO]
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import ReceivesTreeUpdatesHandler._

  override def receive: Receive = withStore(Store())

  private def withStore(store: Store): Receive = {
    case AcquireCapability(
          client,
          CapabilityRegistration(ReceivesTreeUpdates(path))
        ) =>
      store.getWatcher(path) match {
        case Some(watcher) =>
          watcher.forward(
            PathWatcherProtocol.WatchPath(path, client.rpcController)
          )
        case None =>
          val watcher =
            context.actorOf(PathWatcher.props(config, fs, exec))
          context.watch(watcher)
          watcher.forward(
            PathWatcherProtocol.WatchPath(path, client.rpcController)
          )
          context.become(withStore(store.addWatcher(watcher, path)))
      }

    case ReleaseCapability(
          client,
          CapabilityRegistration(ReceivesTreeUpdates(path))
        ) =>
      store.getWatcher(path) match {
        case Some(watcher) =>
          watcher.forward(PathWatcherProtocol.UnwatchPath(client.rpcController))
        case None =>
          sender() ! CapabilityNotAcquiredResponse
      }

    case Terminated(watcher) =>
      context.become(withStore(store.removeWatcher(watcher)))
  }
}

object ReceivesTreeUpdatesHandler {

  /** Internal state of a [[ReceivesTreeUpdatesHandler]].
    *
    * @param watchers mappings of a path watcher with a path
    */
  case class Store(watchers: Map[Path, ActorRef]) {

    /** Returns watcher associated with the provided path.
      *
      * @param path watched path
      * @return optional watcher associated with this path
      */
    def getWatcher(path: Path): Option[ActorRef] =
      watchers.get(path)

    /** Add new watcher with the path to the store.
      *
      * @param watcher path watcher
      * @param path watched path
      * @return updated store
      */
    def addWatcher(watcher: ActorRef, path: Path): Store =
      copy(watchers = watchers + (path -> watcher))

    /** Remove watcher from the store.
      *
      * @param watcher path watcher
      * @return updated store
      */
    def removeWatcher(watcher: ActorRef): Store =
      copy(watchers = watchers.filter(kv => kv._2 != watcher))
  }

  private object Store {

    def apply(): Store =
      new Store(Map())
  }

  /** Creates a configuration object used to create a
    * [[ReceivesTreeUpdatesHandler]].
    *
    * @param config configuration
    * @param fs file system
    * @param exec executor of file system events
    */
  def props(
    config: Config,
    fs: FileSystemApi[BlockingIO],
    exec: Exec[BlockingIO]
  ): Props =
    Props(new ReceivesTreeUpdatesHandler(config, fs, exec))
}
