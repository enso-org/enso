package org.enso.syntax2.serialization;

public class FormatException
  extends RuntimeException {
    public FormatException(String errorMessage, Throwable err) {
        super(errorMessage, err);
    }
    public FormatException(String errorMessage) {
        super(errorMessage);
    }
}
