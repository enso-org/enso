package org.enso.runner

import cats.implicits.toTraverseOps
import com.typesafe.scalalogging.Logger
import org.enso.cli.ProgressBar
import org.enso.cli.task.{ProgressReporter, TaskProgress}
import org.enso.distribution.locking.{
  LockUserInterface,
  Resource,
  ResourceManager,
  ThreadSafeFileLockManager
}
import org.enso.distribution.{DistributionManager, Environment, LanguageHome}
import org.enso.editions.updater.EditionManager
import org.enso.editions.{DefaultEdition, EditionResolver}
import org.enso.languageserver.libraries.CompilerBasedDependencyExtractor
import org.enso.librarymanager.dependencies.DependencyResolver
import org.enso.librarymanager.{DefaultLibraryProvider, LibraryResolver}
import org.enso.loggingservice.LogLevel
import org.enso.pkg.PackageManager

import java.io.File

/** A helper to preinstall all dependencies of a project. */
object DependencyPreinstaller {

  /** Parses the project to find out its direct dependencies, uses the resolver
    * to find all transitive dependencies and ensures that all of them are
    * installed.
    */
  def preinstallDependencies(projectRoot: File, logLevel: LogLevel): Unit = {
    val logger = Logger[DependencyPreinstaller.type]
    val pkg    = PackageManager.Default.loadPackage(projectRoot).get

    val dependencyExtractor = new CompilerBasedDependencyExtractor(logLevel)
    val environment         = new Environment {}
    val languageHome        = LanguageHome.detectFromExecutableLocation(environment)

    val distributionManager = new DistributionManager(environment)
    val lockManager = new ThreadSafeFileLockManager(
      distributionManager.paths.locks
    )
    val resourceManager = new ResourceManager(lockManager)

    val editionProvider = EditionManager.makeEditionProvider(
      distributionManager,
      Some(languageHome)
    )
    val editionResolver = EditionResolver(editionProvider)
    val edition = editionResolver
      .resolve(
        pkg.getConfig().edition.getOrElse(DefaultEdition.getDefaultEdition)
      ) match {
      case Left(error) =>
        throw new RuntimeException(
          s"Cannot resolve current project's edition: ${error.getMessage}"
        )
      case Right(value) => value
    }

    val preferLocalLibraries = pkg.getConfig().preferLocalLibraries

    val (localLibraryProvider, publishedLibraryProvider) =
      DefaultLibraryProvider.makeProviders(
        distributionManager,
        resourceManager,
        new LockUserInterface {
          override def startWaitingForResource(resource: Resource): Unit =
            logger.warn(resource.waitMessage)

          override def finishWaitingForResource(resource: Resource): Unit = ()
        },
        new ProgressReporter {
          override def trackProgress(
            message: String,
            task: TaskProgress[_]
          ): Unit = {
            logger.info(message)
            ProgressBar.waitWithProgress(task)
          }
        },
        languageHome = Some(languageHome),
        projectRoot  = Some(projectRoot.toPath)
      )

    val dependencyResolver = new DependencyResolver(
      localLibraryProvider,
      publishedLibraryProvider,
      edition,
      preferLocalLibraries,
      LibraryResolver(localLibraryProvider),
      dependencyExtractor
    )
    val installer = new DefaultLibraryProvider(
      localLibraryProvider,
      publishedLibraryProvider,
      edition,
      preferLocalLibraries
    )
    val immediateDependencies = dependencyExtractor.findDependencies(pkg)
    logger.trace(
      s"The project imports the following libraries: $immediateDependencies."
    )
    val allDependencies = immediateDependencies.flatMap { name =>
      dependencyResolver.findDependencies(name).get
    }
    logger.trace(s"The project depends on: $allDependencies.")

    val dependenciesToInstall = allDependencies.filter(!_.isCached)

    if (dependenciesToInstall.isEmpty) {
      logger.info(s"All ${allDependencies.size} dependencies are installed.")
    } else {
      logger.info(s"Will install ${dependenciesToInstall.size} dependencies.")
      val result = dependenciesToInstall.toList.traverse { dependency =>
        installer.findSpecificLibraryVersion(
          dependency.libraryName,
          dependency.version
        )
      }
      result match {
        case Left(error) =>
          throw new RuntimeException(
            s"Some dependencies could not be installed: [$error]."
          )
        case Right(_) =>
      }
    }
  }
}
