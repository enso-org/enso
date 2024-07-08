package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.util.concurrent.Executor;
import org.enso.common.LanguageInfo;
import org.enso.languageserver.boot.ComponentSupervisor;
import org.enso.languageserver.event.InitializedEvent;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialize the Truffle context. */
public class TruffleContextInitialization extends LockedInitialization {

  private final Context.Builder truffleContextBuilder;
  private final ComponentSupervisor supervisor;
  private final EventStream eventStream;

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  /**
   * Creates an instance of Truffle initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param eventStream the events stream
   * @param truffleContextBuilder the Truffle context builder
   */
  public TruffleContextInitialization(
      Executor executor,
      Context.Builder truffleContextBuilder,
      ComponentSupervisor supervisor,
      EventStream eventStream) {
    super(executor);
    this.truffleContextBuilder = truffleContextBuilder;
    this.supervisor = supervisor;
    this.eventStream = eventStream;
  }

  @Override
  public void initComponent() {
    logger.trace("Creating Runtime context.");
    if (Engine.newBuilder()
        .allowExperimentalOptions(true)
        .build()
        .getLanguages()
        .containsKey("java")) {
      truffleContextBuilder
          .option("java.ExposeNativeJavaVM", "true")
          .option("java.Polyglot", "true")
          .option("java.UseBindingsLoader", "true")
          .allowCreateThread(true);
    }
    var truffleContext = truffleContextBuilder.build();
    supervisor.registerService(truffleContext);
    logger.trace("Created Runtime context [{}].", truffleContext);
    logger.info("Initializing Runtime context [{}]...", truffleContext);
    truffleContext.initialize(LanguageInfo.ID);
    eventStream.publish(InitializedEvent.TruffleContextInitialized$.MODULE$);
    logger.info("Initialized Runtime context [{}].", truffleContext);
  }
}
