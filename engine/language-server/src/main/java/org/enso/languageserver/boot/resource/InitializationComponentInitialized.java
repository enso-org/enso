package org.enso.languageserver.boot.resource;

public final class InitializationComponentInitialized {

  private static final class InstanceHolder {
    private static final InitializationComponentInitialized INSTANCE =
        new InitializationComponentInitialized();
  }

  public static InitializationComponentInitialized getInstance() {
    return InstanceHolder.INSTANCE;
  }
}
