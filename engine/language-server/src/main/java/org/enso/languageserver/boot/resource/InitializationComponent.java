package org.enso.languageserver.boot.resource;

import java.util.concurrent.CompletableFuture;

/** A component that should be initialized. */
public interface InitializationComponent {

  /** @return `true` if the component is initialized */
  boolean isInitialized();

  /** Initialize the component. */
  CompletableFuture<Void> init();
}
