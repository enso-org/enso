package org.enso.languageserver.libraries.handler

import akka.actor.{Actor, ActorRef, Props, Status}
import akka.pattern.pipe
import com.typesafe.scalalogging.LazyLogging
import org.enso.editions.{LibraryName, LibraryVersion}
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult}
import org.enso.languageserver.filemanager.FileManagerApi.FileSystemError
import org.enso.languageserver.libraries.LibraryApi._
import org.enso.languageserver.libraries.{
  BlockingOperation,
  ComponentGroupsResolver,
  EditionReferenceResolver,
  LibraryComponentGroup
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.pkg.Config

/** A request handler for the `editions/listDefinedComponents` endpoint.
  *
  * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
  * @param localLibraryProvider     a provider of local libraries
  * @param publishedLibraryCache    a cache of published libraries
  * @param componentGroupsResolver  a module resolving the dependencies between
  *                                 component groups
  */
class EditionsListDefinedComponentsHandler(
  editionReferenceResolver: EditionReferenceResolver,
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryCache: PublishedLibraryCache,
  componentGroupsResolver: ComponentGroupsResolver
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(
          EditionsListDefinedComponents,
          id,
          EditionsListDefinedComponents.Params(reference)
        ) =>
      BlockingOperation
        .run {
          val edition = editionReferenceResolver.resolveEdition(reference).get
          val definedLibraries = edition.getAllDefinedLibraries.view
            .map { case (name, version) =>
              readLocalPackage(name, version)
            }
            .collect { case Some(config) =>
              config
            }
          componentGroupsResolver.run(definedLibraries)
        }
        .map(EditionsListDefinedComponentsHandler.Result) pipeTo self

      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case EditionsListDefinedComponentsHandler.Result(components) =>
      replyTo ! ResponseResult(
        EditionsListDefinedComponents,
        id,
        EditionsListDefinedComponents.Result(components)
      )
      context.stop(self)

    case Status.Failure(exception) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemError(exception.getMessage)
      )
      context.stop(self)
  }

  private def readLocalPackage(
    libraryName: LibraryName,
    libraryVersion: LibraryVersion
  ): Option[Config] = {
    val libraryPathOpt = libraryVersion match {
      case LibraryVersion.Local =>
        localLibraryProvider.findLibrary(libraryName)
      case LibraryVersion.Published(version, _) =>
        publishedLibraryCache.findCachedLibrary(libraryName, version)
    }
    libraryPathOpt.flatMap { libraryPath =>
      libraryPath.getReadAccess.readPackage().toOption
    }
  }
}
object EditionsListDefinedComponentsHandler {

  private case class Result(components: Seq[LibraryComponentGroup])

  /** Creates a configuration object to create
    * [[EditionsListDefinedComponentsHandler]].
    *
    * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
    * @param localLibraryProvider     a provider of local libraries
    * @param publishedLibraryCache    a cache of published libraries
    * @param componentGroupsResolver  a module resolving the dependencies
    *                                 between component groups
    */
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    localLibraryProvider: LocalLibraryProvider,
    publishedLibraryCache: PublishedLibraryCache,
    componentGroupsResolver: ComponentGroupsResolver =
      new ComponentGroupsResolver
  ): Props = Props(
    new EditionsListDefinedComponentsHandler(
      editionReferenceResolver,
      localLibraryProvider,
      publishedLibraryCache,
      componentGroupsResolver
    )
  )
}
