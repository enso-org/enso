package org.enso.languageserver.boot.resource;

import java.util.concurrent.Executor;
import org.enso.languageserver.data.ProjectDirectoriesConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Directories initialization. */
public class DirectoriesInitialization extends LockedInitialization {
  private final ProjectDirectoriesConfig projectDirectoriesConfig;
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  /**
   * Creates the directories initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param projectDirectoriesConfig the directories config
   */
  public DirectoriesInitialization(
      Executor executor, ProjectDirectoriesConfig projectDirectoriesConfig) {
    super(executor);
    this.projectDirectoriesConfig = projectDirectoriesConfig;
  }

  @Override
  public void initComponent() {
    logger.info("Initializing directories...");
    projectDirectoriesConfig.createDirectories();
    logger.info("Initialized directories.");
  }
}
