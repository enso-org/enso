package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.libraryupload.DependencyExtractor
import org.enso.loggingservice.{JavaLoggingLogHandler, LogLevel}
import org.enso.pkg.Package
import org.enso.pkg.SourceFile
import org.enso.polyglot.{PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.Context

import java.io.File

class CompilerBasedDependencyExtractor(logLevel: LogLevel)
    extends DependencyExtractor[File] {

  /** @inheritdoc */
  override def findDependencies(pkg: Package[File]): Set[LibraryName] = {
    val context = createContextWithProject(pkg)

    def findImportedLibraries(file: SourceFile[File]): Set[LibraryName] = {
      val module  = context.getTopScope.getModule(file.qualifiedName.toString)
      val imports = module.gatherImportStatements()
      val importedLibraries = imports.map { rawName =>
        LibraryName.fromString(rawName) match {
          case Left(error) =>
            throw new IllegalStateException(error)
          case Right(value) => value
        }
      }
      importedLibraries.toSet
    }

    val sourcesImports = pkg.listSources.toSet.flatMap(findImportedLibraries)
    val itself         = pkg.libraryName

    sourcesImports - itself
  }

  private def createContextWithProject(pkg: Package[File]): PolyglotContext = {
    val context = Context
      .newBuilder()
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .option(RuntimeOptions.PROJECT_ROOT, pkg.root.getCanonicalPath)
      .option("js.foreign-object-prototype", "true")
      .option(
        RuntimeOptions.LOG_LEVEL,
        JavaLoggingLogHandler.getJavaLogLevelFor(logLevel).getName
      )
      .logHandler(
        JavaLoggingLogHandler.create(JavaLoggingLogHandler.defaultLevelMapping)
      )
      .build
    new PolyglotContext(context)
  }
}
