package org.enso.languageserver.boot.resource;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import org.enso.jsonrpc.ProtocolFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialization of JSON-RPC protocol. */
public class JsonRpcInitialization implements InitializationComponent {

  private final Executor executor;
  private final ProtocolFactory protocolFactory;
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  private volatile boolean isInitialized = false;

  /**
   * Create an instance of JSON-RPC initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param protocolFactory the JSON-RPC protocol factory
   */
  public JsonRpcInitialization(Executor executor, ProtocolFactory protocolFactory) {
    this.executor = executor;
    this.protocolFactory = protocolFactory;
  }

  @Override
  public boolean isInitialized() {
    return isInitialized;
  }

  @Override
  public CompletableFuture<Void> init() {
    return CompletableFuture.runAsync(
        () -> {
          logger.info("Initializing JSON-RPC protocol.");
          protocolFactory.init();
          logger.info("JSON-RPC protocol initialized.");
          isInitialized = true;
        },
        executor);
  }
}
