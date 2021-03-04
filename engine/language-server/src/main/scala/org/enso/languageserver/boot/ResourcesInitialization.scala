package org.enso.languageserver.boot

import akka.event.EventStream
import org.enso.languageserver.boot.resource.{
  DirectoriesInitialization,
  InitializationComponent,
  RepoInitialization,
  SequentialResourcesInitialization,
  TruffleContextInitialization
}
import org.enso.languageserver.data.DirectoriesConfig
import org.enso.searcher.{FileVersionsRepo, SuggestionsRepo}
import org.graalvm.polyglot.Context

import scala.concurrent.{ExecutionContext, Future}

/** Helper object for the initialization of the Language Server resources.
  * Creates the directories, initializes the databases, and the Truffle context.
  */
object ResourcesInitialization {

  /** Create the initialization component of the Language Server.
    *
    * @param eventStream system event stream
    * @param directoriesConfig configuration of directories that should be created
    * @param suggestionsRepo the suggestions repo
    * @param versionsRepo the file versions repo
    * @param truffleContext the runtime context
    * @return the initialization component
    */
  def apply(
    eventStream: EventStream,
    directoriesConfig: DirectoriesConfig,
    suggestionsRepo: SuggestionsRepo[Future],
    versionsRepo: FileVersionsRepo[Future],
    truffleContext: Context
  )(implicit ec: ExecutionContext): InitializationComponent = {
    val resources = Seq(
      new DirectoriesInitialization(directoriesConfig),
      new RepoInitialization(eventStream, suggestionsRepo, versionsRepo),
      new TruffleContextInitialization(truffleContext)
    )
    new SequentialResourcesInitialization(resources)
  }
}
