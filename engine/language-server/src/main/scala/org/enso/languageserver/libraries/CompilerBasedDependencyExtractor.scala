package org.enso.languageserver.libraries

import org.enso.editions.LibraryName
import org.enso.libraryupload.DependencyExtractor
import org.enso.loggingservice.{JavaLoggingLogHandler, LogLevel}
import org.enso.pkg.Package
import org.enso.pkg.SourceFile
import org.enso.polyglot.{HostAccessFactory, PolyglotContext, RuntimeOptions}
import org.graalvm.polyglot.Context

import java.io.File

/** A dependency extractor that runs the compiler in a mode that only parses the
  * source code and runs just the basic preprocessing phases to find out what
  * libraries are imported by the project.
  *
  * @param logLevel the log level to use for the runtime context that will do
  *                 the parsing
  */
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

    val sourcesImports = pkg.listSources().toSet.flatMap(findImportedLibraries)
    val itself         = pkg.libraryName

    // Builtins need to be removed from the set of the dependencies, because
    // even if they are imported, they are not a typical library.
    val builtins = LibraryName("Standard", "Builtins")

    sourcesImports - itself - builtins
  }

  /** Creates a simple runtime context with the given package loaded as its
    * project root.
    */
  private def createContextWithProject(pkg: Package[File]): PolyglotContext = {
    val context = Context
      .newBuilder()
      .allowExperimentalOptions(true)
      .allowAllAccess(true)
      .allowHostAccess(new HostAccessFactory().allWithTypeMapping())
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
