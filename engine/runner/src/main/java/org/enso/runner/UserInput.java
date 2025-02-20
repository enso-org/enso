package org.enso.runner;

sealed class UserInput permits EndOfInput, Line {}

final class EndOfInput extends UserInput {}

final class Line extends UserInput {
  private final String line;

  Line(String line) {
    this.line = line;
  }

  public String getLine() {
    return line;
  }
}
