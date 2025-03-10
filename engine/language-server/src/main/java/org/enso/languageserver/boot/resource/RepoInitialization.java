package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Executor;
import java.util.concurrent.Semaphore;
import org.enso.languageserver.data.ProjectDirectoriesConfig;
import org.enso.languageserver.event.InitializedEvent;
import org.enso.searcher.memory.InMemorySuggestionsRepo;
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
  private final InMemorySuggestionsRepo suggestionsRepo;

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private final Semaphore lock = new Semaphore(1);

  private volatile boolean isInitialized = false;

  /**
   * Create an instance of repo initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param projectDirectoriesConfig configuration of language server directories
   * @param eventStream the events stream
   * @param suggestionsRepo the suggestions repo
   */
  public RepoInitialization(
      Executor executor,
      ProjectDirectoriesConfig projectDirectoriesConfig,
      EventStream eventStream,
      InMemorySuggestionsRepo
          suggestionsRepo) { // Java won't allow Future type constructor in SuggestionsRepo[Future]
    this.executor = executor;
    this.projectDirectoriesConfig = projectDirectoriesConfig;
    this.eventStream = eventStream;
    this.suggestionsRepo = suggestionsRepo;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  @Override
  public CompletableFuture<Void> init() {
    return initSuggestionsRepo()
        .whenCompleteAsync(
            (res, err) -> {
              if (err == null) {
                isInitialized = true;
              }
              lock.release();
            },
            executor);
  }

  private CompletableFuture<Void> initSuggestionsRepo() {
    return CompletableFuture.supplyAsync(
            () -> {
              logger.debug("Initializing Suggestions repo [{}]...", suggestionsRepo);
              try {
                lock.acquire();
                if (!isInitialized)
                  return doInitSuggestionsRepo()
                      .exceptionallyComposeAsync(this::recoverInitializationError, executor);
                else return CompletableFuture.completedFuture(null);
              } catch (InterruptedException e) {
                throw new RuntimeException(e);
              }
            },
            executor)
        .thenRunAsync(
            () -> logger.debug("Initialized Suggestions repo [{}]", suggestionsRepo), executor)
        .whenCompleteAsync(
            (res, err) -> {
              if (err != null) {
                logger.error(
                    "Failed to initialize SQL suggestions repo [{}]", suggestionsRepo, err);
              } else {
                eventStream.publish(InitializedEvent.SuggestionsRepoInitialized$.MODULE$);
              }
            });
  }

  private CompletableFuture<Void> recoverInitializationError(Throwable error) {
    return CompletableFuture.runAsync(
            () ->
                logger.warn(
                    "Failed to initialize the suggestions database [{}]", suggestionsRepo, error),
            executor)
        .thenRunAsync(() -> logger.info("Retrying suggestions repo initialization"), executor)
        .thenComposeAsync(v -> doInitSuggestionsRepo(), executor);
  }

  private CompletionStage<Void> doInitSuggestionsRepo() {
    return FutureConverters.asJava(suggestionsRepo.init()).thenAcceptAsync(res -> {}, executor);
  }
}
