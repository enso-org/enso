package org.enso.table.error;

public class InvalidColumnNameException extends RuntimeException {
  public final String name;
  public final String extraMessage;

  public InvalidColumnNameException(String name, String extraMessage) {
    this.name = name;
    this.extraMessage = extraMessage;
  }

  @Override
  public String getMessage() {
    String message = "Invalid column name: " + name + ".";
    if (extraMessage != null) {
      message += " " + extraMessage;
    }
    return message;
  }
}
