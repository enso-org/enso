package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.io.IOException;
import java.nio.file.FileSystemException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executor;
import org.apache.commons.io.FileUtils;
import org.enso.languageserver.data.ProjectDirectoriesConfig;
import org.enso.languageserver.event.InitializedEvent;
import org.enso.logger.masking.MaskedPath;
import org.enso.searcher.sql.SqlDatabase;
import org.enso.searcher.sql.SqlSuggestionsRepo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.FutureConverters;

/** Initialization of the Language Server suggestions database. */
public class RepoInitialization implements InitializationComponent {

  private static final int MAX_RETRIES = 3;
  private static final long RETRY_DELAY_MILLIS = 1000;

  private final Executor executor;

  private final ProjectDirectoriesConfig projectDirectoriesConfig;
  private final EventStream eventStream;
  private final SqlDatabase sqlDatabase;
  private final SqlSuggestionsRepo sqlSuggestionsRepo;

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private volatile boolean isInitialized = false;

  /**
   * Create an instance of repo initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param projectDirectoriesConfig configuration of language server directories
   * @param eventStream the events stream
   * @param sqlDatabase the sql database
   * @param sqlSuggestionsRepo the suggestions repo
   */
  public RepoInitialization(
      Executor executor,
      ProjectDirectoriesConfig projectDirectoriesConfig,
      EventStream eventStream,
      SqlDatabase sqlDatabase,
      SqlSuggestionsRepo sqlSuggestionsRepo) {
    this.executor = executor;
    this.projectDirectoriesConfig = projectDirectoriesConfig;
    this.eventStream = eventStream;
    this.sqlDatabase = sqlDatabase;
    this.sqlSuggestionsRepo = sqlSuggestionsRepo;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  @Override
  public CompletableFuture<Void> init() {
    return initSqlDatabase()
        .thenComposeAsync(v -> initSuggestionsRepo(), executor)
        .thenRunAsync(() -> isInitialized = true, executor);
  }

  private CompletableFuture<Void> initSqlDatabase() {
    return CompletableFuture.runAsync(
            () -> {
              logger.info("Initializing sql database [{}]...", sqlDatabase);
              sqlDatabase.open();
              logger.info("Initialized sql database [{}].", sqlDatabase);
            },
            executor)
        .whenCompleteAsync(
            (res, err) -> {
              if (err != null) {
                logger.error("Failed to initialize sql database [{}].", sqlDatabase, err);
              }
            },
            executor);
  }

  private CompletableFuture<Void> initSuggestionsRepo() {
    return CompletableFuture.runAsync(
            () -> logger.info("Initializing suggestions repo [{}]...", sqlDatabase), executor)
        .thenComposeAsync(
            v ->
                doInitSuggestionsRepo()
                    .exceptionallyComposeAsync(this::recoverInitializationError, executor),
            executor)
        .thenRunAsync(
            () -> logger.info("Initialized Suggestions repo [{}].", sqlDatabase), executor)
        .whenCompleteAsync(
            (res, err) -> {
              if (err != null) {
                logger.error("Failed to initialize SQL suggestions repo [{}].", sqlDatabase, err);
              } else {
                eventStream.publish(InitializedEvent.SuggestionsRepoInitialized$.MODULE$);
              }
            });
  }

  private CompletableFuture<Void> recoverInitializationError(Throwable error) {
    return CompletableFuture.runAsync(
            () ->
                logger.warn(
                    "Failed to initialize the suggestions database [{}].", sqlDatabase, error),
            executor)
        .thenRunAsync(sqlDatabase::close, executor)
        .thenComposeAsync(v -> clearDatabaseFile(0), executor)
        .thenRunAsync(sqlDatabase::open, executor)
        .thenRunAsync(() -> logger.info("Retrying database initialization."), executor)
        .thenComposeAsync(v -> doInitSuggestionsRepo(), executor);
  }

  private CompletableFuture<Void> clearDatabaseFile(int retries) {
    return CompletableFuture.runAsync(
            () -> {
              logger.info("Clear database file. Attempt #{}.", retries + 1);
              try {
                Files.delete(projectDirectoriesConfig.suggestionsDatabaseFile().toPath());
              } catch (IOException e) {
                throw new CompletionException(e);
              }
            },
            executor)
        .exceptionallyComposeAsync(error -> recoverClearDatabaseFile(error, retries), executor);
  }

  private CompletableFuture<Void> recoverClearDatabaseFile(Throwable error, int retries) {
    if (error instanceof CompletionException) {
      return recoverClearDatabaseFile(error.getCause(), retries);
    } else if (error instanceof NoSuchFileException) {
      logger.warn(
          "Failed to delete the database file. Attempt #{}. File does not exist [{}].",
          retries + 1,
          new MaskedPath(projectDirectoriesConfig.suggestionsDatabaseFile().toPath()));
      return CompletableFuture.completedFuture(null);
    } else if (error instanceof FileSystemException) {
      logger.error(
          "Failed to delete the database file. Attempt #{}. The file will be removed during the shutdown.",
          retries + 1,
          error);
      Runtime.getRuntime()
          .addShutdownHook(
              new Thread(
                  () ->
                      FileUtils.deleteQuietly(projectDirectoriesConfig.suggestionsDatabaseFile())));
      return CompletableFuture.failedFuture(error);
    } else if (error instanceof IOException) {
      logger.error("Failed to delete the database file. Attempt #{}.", retries + 1, error);
      if (retries < MAX_RETRIES) {
        try {
          Thread.sleep(RETRY_DELAY_MILLIS);
        } catch (InterruptedException e) {
          throw new CompletionException(e);
        }
        return clearDatabaseFile(retries + 1);
      } else {
        return CompletableFuture.failedFuture(error);
      }
    }

    return CompletableFuture.completedFuture(null);
  }

  private CompletionStage<Void> doInitSuggestionsRepo() {
    return FutureConverters.asJava(sqlSuggestionsRepo.init()).thenAcceptAsync(res -> {}, executor);
  }
}
