package org.enso.base.enso_cloud.logging;

public interface LogMessage {
  /**
   * @return JSON string representation of the log message.
   */
  String payload();
}
