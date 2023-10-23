package org.enso.languageserver.boot.resource;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import org.enso.languageserver.data.ProjectDirectoriesConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Directories initialization. */
public class DirectoriesInitialization implements InitializationComponent {

  private final Executor executor;
  private final ProjectDirectoriesConfig projectDirectoriesConfig;
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private volatile boolean isInitialized = false;

  /**
   * Creates the directories initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param projectDirectoriesConfig the directories config
   */
  public DirectoriesInitialization(
      Executor executor, ProjectDirectoriesConfig projectDirectoriesConfig) {
    this.executor = executor;
    this.projectDirectoriesConfig = projectDirectoriesConfig;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  @Override
  public CompletableFuture<Void> init() {
    return CompletableFuture.runAsync(
        () -> {
          logger.info("Initializing directories...");
          projectDirectoriesConfig.createDirectories();
          logger.info("Initialized directories.");
          isInitialized = true;
        },
        executor);
  }
}
