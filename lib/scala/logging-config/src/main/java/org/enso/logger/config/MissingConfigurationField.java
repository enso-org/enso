package org.enso.logger.config;

public class MissingConfigurationField extends Exception {
  public MissingConfigurationField(String name) {
    super("Missing required configuration for field `" + name + "`");
  }
}
