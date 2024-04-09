package org.enso.languageserver.boot.resource;

/** Object indicating that the initialization is complete. */
public final class InitializationComponentInitialized {

  private static final class InstanceHolder {
    private static final InitializationComponentInitialized INSTANCE =
        new InitializationComponentInitialized();
  }

  /**
   * Get the initialized marker object.
   *
   * @return the instance of {@link InitializationComponentInitialized}.
   */
  public static InitializationComponentInitialized getInstance() {
    return InstanceHolder.INSTANCE;
  }
}
