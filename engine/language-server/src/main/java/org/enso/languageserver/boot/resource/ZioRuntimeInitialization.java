package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import org.enso.languageserver.effect.Runtime;
import org.enso.languageserver.event.InitializedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialization of ZIO runtime. */
public class ZioRuntimeInitialization implements InitializationComponent {

  private final Executor executor;
  private final Runtime runtime;
  private final EventStream eventStream;
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private volatile boolean isInitialized = false;

  /**
   * Create an instance of ZIO runtime initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param runtime the runtime to initialize
   * @param eventStream the events stream
   */
  public ZioRuntimeInitialization(Executor executor, Runtime runtime, EventStream eventStream) {
    this.executor = executor;
    this.runtime = runtime;
    this.eventStream = eventStream;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  @Override
  public CompletableFuture<Void> init() {
    return CompletableFuture.runAsync(
        () -> {
          logger.info("Initializing ZIO runtime...");
          runtime.init();
          logger.info("ZIO runtime initialized [{}].", runtime);
          isInitialized = true;
          eventStream.publish(InitializedEvent.ZioRuntimeInitialized$.MODULE$);
        },
        executor);
  }
}
