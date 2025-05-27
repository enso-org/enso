package org.enso.base.encoding;

public record DecodingProblem(String message) {
  public String getTheMessage() {
    return this.message;
  }
}
