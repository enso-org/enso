package org.enso.languageserver.filemanager

import akka.actor.{Actor, ActorRef, Props, Stash, Terminated}
import com.typesafe.scalalogging.LazyLogging
import org.enso.filewatcher.WatcherFactory
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
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.PathWatcher.{
  ForwardRequest,
  ForwardResponse
}
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
  * @param contentRootManager the content root manager
  * @param watcherFactory the factory creating the file watcher
  * @param fs file system
  * @param exec executor of file system events
  */
final class ReceivesTreeUpdatesHandler(
  config: Config,
  contentRootManager: ContentRootManager,
  watcherFactory: WatcherFactory,
  fs: FileSystemApi[BlockingIO],
  exec: Exec[BlockingIO]
) extends Actor
    with Stash
    with LazyLogging
    with UnhandledLogging {

  import ReceivesTreeUpdatesHandler._

  override def preStart(): Unit = {
    super.preStart()

    context.system.eventStream
      .subscribe(self, InitializedEvent.ZioRuntimeInitialized.getClass)

    self ! WatchProjectContentRoot
  }

  override def receive: Receive = initializing

  private def initializing: Receive = {
    case InitializedEvent.ZioRuntimeInitialized =>
      logger.info("Initialized")
      unstashAll()
      context.become(withStore(Store()))

    case _ =>
      stash()
  }

  private def withStore(store: Store): Receive = {
    case WatchProjectContentRoot =>
      val projectContentRoot =
        Path(config.projectContentRoot.contentRoot.id, Vector())
      logger.debug("Watch project content root [{}]", projectContentRoot)
      if (store.getWatcher(projectContentRoot).isEmpty) {
        val watcher =
          context.actorOf(
            PathWatcher.props(
              config.pathWatcher,
              contentRootManager,
              watcherFactory,
              fs,
              exec
            )
          )
        context.watch(watcher)
        watcher.forward(
          PathWatcherProtocol.WatchPath(projectContentRoot, Set[ActorRef]())
        )
        context.become(
          withStore(store.addWatcher(watcher, projectContentRoot))
        )
      }

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
            context.actorOf(
              PathWatcher.props(
                config.pathWatcher,
                contentRootManager,
                watcherFactory,
                fs,
                exec
              )
            )
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
          watcher ! ForwardRequest(
            sender(),
            PathWatcherProtocol.UnwatchPath(client.rpcController)
          )
        case None =>
          sender() ! CapabilityNotAcquiredResponse
      }

    case ForwardResponse(client, response, isLast) =>
      client.forward(response)
      if (isLast) {
        context.become(withStore(store.removeWatcher(sender())))
      }

    case Terminated(watcher) =>
      context.become(withStore(store.removeWatcher(watcher)))
  }
}

object ReceivesTreeUpdatesHandler {

  private object WatchProjectContentRoot

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
    * @param contentRootManager the content root manager
    * @param watcherFactory the factory creating the file watcher
    * @param fs file system
    * @param exec executor of file system events
    */
  def props(
    config: Config,
    contentRootManager: ContentRootManager,
    watcherFactory: WatcherFactory,
    fs: FileSystemApi[BlockingIO],
    exec: Exec[BlockingIO]
  ): Props =
    Props(
      new ReceivesTreeUpdatesHandler(
        config,
        contentRootManager,
        watcherFactory,
        fs,
        exec
      )
    )
}
