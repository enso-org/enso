package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.ActorRef
import org.enso.projectmanager.infrastructure.languageserver.LanguageServerExecutor.LifecycleListener

/** A service responsible for executing the forked language server process. */
trait LanguageServerExecutor {

  /** Starts the language server process in a background thread.
    *
    * @param descriptor options related to this language server instance
    * @param progressTracker reference to an actor that should be notifed of any
    *                        locks
    * @param rpcPort port to use for the RPC channel
    * @param secureRpcPort port to use for the RPC channel
    * @param secureDataPort port to use for the binary channel
    * @param lifecycleListener a listener that will be notified when the process
    *                          is started and terminated
    */
  def spawn(
    descriptor: LanguageServerDescriptor,
    progressTracker: ActorRef,
    rpcPort: Int,
    secureRpcPort: Option[Int],
    dataPort: Int,
    secureDataPort: Option[Int],
    lifecycleListener: LifecycleListener
  ): Unit
}

object LanguageServerExecutor {

  /** Listens for lifecycle updates of a language server process. */
  trait LifecycleListener {

    /** Called when the process has been successfully started.
      *
      * @param processHandle a handle that can be used to terminate the process
      */
    def onStarted(processHandle: ProcessHandle): Unit

    /** Called when the process has terminated (either on request or abruptly).
      *
      * @param exitCode exit code of the child process
      */
    def onTerminated(exitCode: Int): Unit

    /** Called when the process fails to start or its execution terminates with
      * an unexpected exception.
      */
    def onFailed(throwable: Throwable): Unit
  }

  /** A handle to the running Language Server child process. */
  trait ProcessHandle {

    /** Requests the child process to terminate gracefully. */
    def requestGracefulTermination(): Unit

    /** Tries to forcibly kill the process. */
    def kill(): Unit
  }
}
