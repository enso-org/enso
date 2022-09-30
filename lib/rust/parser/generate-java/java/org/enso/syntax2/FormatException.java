package org.enso.syntax2;

public class FormatException extends RuntimeException {
  public FormatException(Message m, String errorMessage) {
    this("At " + m.getLocation() + ":" + errorMessage);
  }

  public FormatException(String errorMessage, Throwable err) {
    super(errorMessage, err);
  }

  public FormatException(String errorMessage) {
    super(errorMessage);
  }
}
