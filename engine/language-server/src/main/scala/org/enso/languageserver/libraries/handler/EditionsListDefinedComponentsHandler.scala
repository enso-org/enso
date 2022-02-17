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
  EditionReferenceResolver,
  LibraryComponentGroup
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.librarymanager.local.LocalLibraryProvider
import org.enso.librarymanager.published.PublishedLibraryCache
import org.enso.pkg.{
  ComponentGroup,
  ComponentGroups,
  ExtendedComponentGroup,
  ModuleReference
}

/** A request handler for the `editions/listDefinedComponents` endpoint.
  *
  * @param editionReferenceResolver an [[EditionReferenceResolver]] instance
  * @param localLibraryProvider     a provider of local libraries
  * @param publishedLibraryCache    a cache of published libraries
  */
class EditionsListDefinedComponentsHandler(
  editionReferenceResolver: EditionReferenceResolver,
  localLibraryProvider: LocalLibraryProvider,
  publishedLibraryCache: PublishedLibraryCache
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = requestStage

  import context.dispatcher

  private def requestStage: Receive = {
    case Request(
          EditionsListDefinedComponents,
          id,
          EditionsListDefinedComponents.Params(reference)
        ) =>
      BlockingOperation
        .run {
          val edition = editionReferenceResolver.resolveEdition(reference).get
          val componentGroupsMap = edition.getAllDefinedLibraries.view
            .map { case (name, version) =>
              name -> readLocalPackage(name, version)
            }
            .collect { case (name, Some(componentGroups)) =>
              name -> componentGroups
            }
            .toMap
          resolveComponentGroups(componentGroupsMap)
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
  ): Option[ComponentGroups] = {
    val libraryPathOpt = libraryVersion match {
      case LibraryVersion.Local =>
        localLibraryProvider.findLibrary(libraryName)
      case LibraryVersion.Published(version, _) =>
        publishedLibraryCache.findCachedLibrary(libraryName, version)
    }
    libraryPathOpt
      .flatMap { libraryPath =>
        libraryPath.getReadAccess.readPackage().toOption
      }
      .flatMap { config =>
        config.componentGroups.toOption
      }
  }

  private def resolveComponentGroups(
    libraryComponents: Map[LibraryName, ComponentGroups]
  ): Seq[LibraryComponentGroup] = {
    val newLibraryComponentGroups =
      libraryComponents.view
        .flatMap { case (libraryName, componentGroups) =>
          componentGroups.newGroups.map(toLibraryComponentGroup(libraryName, _))
        }
        .map { libraryComponentGroup =>
          val moduleReference = ModuleReference(
            libraryComponentGroup.library,
            libraryComponentGroup.module
          )
          moduleReference -> libraryComponentGroup
        }
        .toMap
    val extendedComponentGroups = {
      libraryComponents.view
        .flatMap { case (_, componentGroups) =>
          componentGroups.extendedGroups
        }
        .map { extendedComponentGroup =>
          extendedComponentGroup.module -> extendedComponentGroup
        }
        .toMap
    }
    mergeExtendedComponentGroups(
      newLibraryComponentGroups,
      extendedComponentGroups
    )
  }

  private def toLibraryComponentGroup(
    libraryName: LibraryName,
    componentGroup: ComponentGroup
  ): LibraryComponentGroup = {
    LibraryComponentGroup(
      libraryName,
      componentGroup.module,
      componentGroup.color,
      componentGroup.icon,
      componentGroup.exports
    )
  }

  private def toLibraryComponentGroup(
    componentGroup: ExtendedComponentGroup
  ): LibraryComponentGroup = {
    LibraryComponentGroup(
      componentGroup.module.libraryName,
      componentGroup.module.moduleName,
      componentGroup.color,
      componentGroup.icon,
      componentGroup.exports
    )
  }

  private def mergeExtendedComponentGroups(
    libraryComponentGroups: Map[ModuleReference, LibraryComponentGroup],
    extendedComponentGroups: Map[ModuleReference, ExtendedComponentGroup]
  ): Seq[LibraryComponentGroup] = {
    val keySet =
      libraryComponentGroups.keySet.union(extendedComponentGroups.keySet)
    keySet
      .foldLeft(Vector.newBuilder[LibraryComponentGroup]) { (builder, key) =>
        (
          libraryComponentGroups.get(key),
          extendedComponentGroups.get(key)
        ) match {
          case (Some(libraryComponentGroup), Some(extendedComponentGroup)) =>
            builder += applyExtendedComponentGroup(
              libraryComponentGroup,
              extendedComponentGroup
            )
          case (Some(libraryComponentGroup), None) =>
            builder += libraryComponentGroup
          case (None, Some(extendedComponentGroup)) =>
            builder += toLibraryComponentGroup(extendedComponentGroup)
          case (None, None) =>
            builder
        }
      }
      .result()
  }

  private def applyExtendedComponentGroup(
    libraryComponentGroup: LibraryComponentGroup,
    extendedComponentGroup: ExtendedComponentGroup
  ): LibraryComponentGroup = {
    libraryComponentGroup.copy(
      color   = extendedComponentGroup.color.orElse(libraryComponentGroup.color),
      icon    = extendedComponentGroup.icon.orElse(libraryComponentGroup.icon),
      exports = libraryComponentGroup.exports :++ extendedComponentGroup.exports
    )
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
    */
  def props(
    editionReferenceResolver: EditionReferenceResolver,
    localLibraryProvider: LocalLibraryProvider,
    publishedLibraryCache: PublishedLibraryCache
  ): Props = Props(
    new EditionsListDefinedComponentsHandler(
      editionReferenceResolver,
      localLibraryProvider,
      publishedLibraryCache
    )
  )
}
