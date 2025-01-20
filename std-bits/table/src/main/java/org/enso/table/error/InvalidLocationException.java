package org.enso.table.error;

public class InvalidLocationException extends RuntimeException {
  private final String location;

  public InvalidLocationException(String location, String errorMessage) {
    super(errorMessage);
    this.location = location;
  }

  public String getLocation() {
    return this.location;
  }
}
