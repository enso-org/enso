package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import org.enso.languageserver.event.InitializedEvent;
import org.enso.polyglot.LanguageInfo;
import org.graalvm.polyglot.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialize the Truffle context. */
public class TruffleContextInitialization implements InitializationComponent {

  private final Executor executor;
  private final Context truffleContext;
  private final EventStream eventStream;

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private volatile boolean isInitialized = false;

  /**
   * Creates an instance of Truffle initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param eventStream the events stream
   * @param truffleContext the Truffle context
   */
  public TruffleContextInitialization(
      Executor executor, Context truffleContext, EventStream eventStream) {
    this.executor = executor;
    this.truffleContext = truffleContext;
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
          logger.info("Initializing Runtime context [{}]...", truffleContext);
          truffleContext.initialize(LanguageInfo.ID);
          eventStream.publish(InitializedEvent.TruffleContextInitialized$.MODULE$);
          logger.info("Initialized Runtime context [{}].", truffleContext);
          isInitialized = true;
        },
        executor);
  }
}
