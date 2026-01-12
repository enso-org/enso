package org.enso.runner;

import java.nio.file.Path;
import java.util.List;
import org.enso.cli.ProgressBar;
import org.enso.cli.task.ProgressReporter;
import org.enso.libraryupload.LibraryUploader;
import org.enso.libraryupload.LibraryUploader$;
import org.enso.libraryupload.auth.NoAuthorization$;
import org.enso.libraryupload.auth.SimpleHeaderToken;
import org.enso.libraryupload.auth.Token;
import org.enso.pkg.PackageManager;
import org.enso.runner.common.CompilerBasedDependencyExtractor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;

final class ProjectUploader {
  private ProjectUploader() {}

  private static final Logger logger = LoggerFactory.getLogger(ProjectUploader.class);

  private static ProgressReporter progressReporter(boolean showProgress) {
    return (message, task) -> {
      logger.info(message);
      if (showProgress) {
        ProgressBar.waitWithProgress(task);
      }
    };
  }

  /**
   * Uploads a project to a library repository
   *
   * @param projectRoot path to the root of the project
   * @param uploadUrl URL of upload endpoint of the repository to upload to
   * @param authToken an optional token used for authentication in the repository
   * @param showProgress specifies if CLI progress bars should be displayed showing progress of
   *     compression and upload
   * @param logLevel the log level to use for the context gathering dependencies
   */
  static void uploadProject(
      Path projectRoot, String uploadUrl, String authToken, boolean showProgress, Level logLevel) {
    var progressReporter = progressReporter(showProgress);

    Token token;
    if (authToken != null) {
      token = new SimpleHeaderToken(authToken);
    } else {
      token = NoAuthorization$.MODULE$;
    }

    var dependencyExtractor = new CompilerBasedDependencyExtractor(logLevel);
    var libraryUploader = new LibraryUploader(dependencyExtractor);
    var uploadedRes =
        libraryUploader.uploadLibrary(projectRoot, uploadUrl, token, progressReporter);
    uploadedRes.get();
  }

  static void createSourceArchive(Path projectRoot, Level logLevel, boolean showProgress) {
    var reporter = progressReporter(showProgress);
    var dependencyExtractor = new CompilerBasedDependencyExtractor(logLevel);
    var libraryUploader = new LibraryUploader(dependencyExtractor);
    libraryUploader.createMainArchive(projectRoot, reporter);
  }

  /**
   * Updates manifest of the project.
   *
   * @param projectRoot path to the root of the project
   * @param logLevel the log level to use for the context gathering dependencies
   */
  static void updateManifest(Path projectRoot, Level logLevel, boolean willCreateSrcArchive) {
    var pkg = PackageManager.Default().loadPackage(projectRoot.toFile()).get();

    var dependencyExtractor = new CompilerBasedDependencyExtractor(logLevel);
    var libraryUploader = new LibraryUploader(dependencyExtractor);
    var archiveName = LibraryUploader$.MODULE$.mainArchiveName();
    List<String> archives = willCreateSrcArchive ? List.of(archiveName) : List.of();
    var uploadedRes = libraryUploader.updateManifest(pkg, archives);
    uploadedRes.get();
  }
}
