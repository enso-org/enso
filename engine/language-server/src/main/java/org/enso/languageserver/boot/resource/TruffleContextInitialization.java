package org.enso.languageserver.boot.resource;

import akka.event.EventStream;
import java.util.concurrent.Executor;
import org.enso.common.ContextFactory;
import org.enso.common.LanguageInfo;
import org.enso.languageserver.boot.ComponentSupervisor;
import org.enso.languageserver.event.InitializedEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialize the Truffle context. */
public class TruffleContextInitialization extends LockedInitialization {

  private final ContextFactory contextFactory;
  private final ComponentSupervisor supervisor;
  private final EventStream eventStream;

  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  /**
   * Creates an instance of Truffle initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param eventStream the events stream
   * @param supervisor supervisor
   * @param factory the Truffle context builder
   */
  public TruffleContextInitialization(
      Executor executor,
      ContextFactory factory,
      ComponentSupervisor supervisor,
      EventStream eventStream) {
    super(executor);
    this.contextFactory = factory;
    this.supervisor = supervisor;
    this.eventStream = eventStream;
  }

  @Override
  public void initComponent() {
    logger.trace("Creating Runtime context");
    var truffleContext = contextFactory.build();
    supervisor.registerService(truffleContext);
    logger.trace("Created Runtime context [{}]", truffleContext);
    logger.debug("Initializing Runtime context [{}]", truffleContext);
    truffleContext.initialize(LanguageInfo.ID);
    eventStream.publish(InitializedEvent.TruffleContextInitialized$.MODULE$);
    logger.debug("Initialized Runtime context [{}]", truffleContext);
  }
}
