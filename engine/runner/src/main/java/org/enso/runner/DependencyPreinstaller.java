package org.enso.runner;

import java.io.File;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import org.enso.cli.ProgressBar;
import org.enso.cli.task.ProgressReporter;
import org.enso.cli.task.TaskProgress;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.Environment;
import org.enso.distribution.LanguageHome;
import org.enso.distribution.locking.LockUserInterface;
import org.enso.distribution.locking.Resource;
import org.enso.distribution.locking.ResourceManager;
import org.enso.distribution.locking.ThreadSafeFileLockManager;
import org.enso.editions.DefaultEdition;
import org.enso.editions.EditionResolver;
import org.enso.editions.updater.EditionManager;
import org.enso.librarymanager.DefaultLibraryProvider;
import org.enso.librarymanager.LibraryResolver;
import org.enso.librarymanager.dependencies.Dependency;
import org.enso.librarymanager.dependencies.DependencyResolver;
import org.enso.librarymanager.published.PublishedLibraryCache;
import org.enso.pkg.PackageManager;
import org.enso.runner.common.CompilerBasedDependencyExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;
import scala.Some;

/** A helper to preinstall all dependencies of a project. */
class DependencyPreinstaller {
  private DependencyPreinstaller() {}

  private static final Logger logger = LoggerFactory.getLogger(DependencyPreinstaller.class);

  static void preinstallDependencies(File projectRoot, Level logLevel) {
    var pkg = PackageManager.Default().loadPackage(projectRoot).get();
    var dependencyExtractor = new CompilerBasedDependencyExtractor(logLevel);
    var environment = new Environment() {};
    var languageHome = LanguageHome.detectFromExecutableLocation(environment);

    var distributionManager = new DistributionManager(environment);
    var lockManager = new ThreadSafeFileLockManager(distributionManager.paths().locks());
    var resourceManager = new ResourceManager(lockManager);
    var editionProvider =
        EditionManager.makeEditionProvider(distributionManager, Some.apply(languageHome), true);
    var editionResolver = new EditionResolver(editionProvider);
    var editionOpt =
        editionResolver.resolve(
            pkg.getConfig().edition().getOrElse(DefaultEdition::getDefaultEdition));
    if (editionOpt.isLeft()) {
      @SuppressWarnings("deprecation")
      var ex = editionOpt.left().get();
      throw new RuntimeException("Cannot resolve current project's editionOpt: " + ex.getMessage());
    }
    @SuppressWarnings("deprecation")
    var edition = editionOpt.right().get();

    var preferLocalLibraries = pkg.getConfig().preferLocalLibraries();
    var progressReporter =
        new ProgressReporter() {
          @Override
          public void trackProgress(String message, TaskProgress<?> task) {
            logger.info(message);
            ProgressBar.waitWithProgress(task);
          }
        };
    var lockUserInterface =
        new LockUserInterface() {
          @Override
          public void startWaitingForResource(Resource resource) {
            logger.warn(resource.waitMessage());
          }

          @Override
          public void finishWaitingForResource(Resource resource) {}
        };
    var providers =
        DefaultLibraryProvider.makeProviders(
            distributionManager,
            resourceManager,
            lockUserInterface,
            progressReporter,
            Some.apply(languageHome),
            Some.apply(projectRoot.toPath()));
    var localLibraryProvider = providers._1;
    var publishedLibraryProvider = providers._2;

    var dependencyResolver =
        new DependencyResolver(
            localLibraryProvider,
            (PublishedLibraryCache) publishedLibraryProvider,
            edition,
            preferLocalLibraries,
            new LibraryResolver(localLibraryProvider),
            dependencyExtractor);
    var installer =
        new DefaultLibraryProvider(
            localLibraryProvider, publishedLibraryProvider, edition, preferLocalLibraries);

    var immediateDependencies = dependencyExtractor.findDependencies(pkg);
    logger.trace("The project imports the following libraries: {}", immediateDependencies);
    Set<Dependency> allDependencies = new HashSet<>();
    immediateDependencies.foreach(
        name -> {
          var transitiveDeps = dependencyResolver.findDependencies(name).get();
          transitiveDeps.foreach(
              transitiveDep -> {
                allDependencies.add(transitiveDep);
                return null;
              });
          return null;
        });
    logger.trace("The project depends on: {}", allDependencies);

    var dependenciesToInstall =
        allDependencies.stream()
            .filter(dep -> !dep.isCached())
            .collect(Collectors.toUnmodifiableSet());
    if (dependenciesToInstall.isEmpty()) {
      logger.info("All {} dependencies are installed", allDependencies.size());
    } else {
      logger.info("Will install {} dependencies", dependenciesToInstall.size());
      for (var depToInstall : dependenciesToInstall) {
        var foundOpt =
            installer.findSpecificLibraryVersion(
                depToInstall.libraryName(), depToInstall.version());
        if (foundOpt.isLeft()) {
          @SuppressWarnings("deprecation")
          var err = foundOpt.left().get();
          throw new RuntimeException("Some dependencies could not be installed: [" + err + "].");
        }
      }
    }
  }
}
