package org.enso.languageserver.boot.resource;

import java.util.concurrent.Executor;
import org.enso.jsonrpc.ProtocolFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Initialization of JSON-RPC protocol. */
public class JsonRpcInitialization extends LockedInitialization {
  private final ProtocolFactory protocolFactory;
  private final Logger logger = LoggerFactory.getLogger(this.getClass());

  /**
   * Create an instance of JSON-RPC initialization component.
   *
   * @param executor the executor that runs the initialization
   * @param protocolFactory the JSON-RPC protocol factory
   */
  public JsonRpcInitialization(Executor executor, ProtocolFactory protocolFactory) {
    super(executor);
    this.protocolFactory = protocolFactory;
  }

  @Override
  public void initComponent() {
    logger.info("Initializing JSON-RPC protocol.");
    protocolFactory.init();
    logger.info("JSON-RPC protocol initialized.");
  }
}
