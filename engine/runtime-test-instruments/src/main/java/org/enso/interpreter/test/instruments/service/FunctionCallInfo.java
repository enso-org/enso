package org.enso.interpreter.test.instruments.service;

public record FunctionCallInfo(
    String moduleName,
    String typeName,
    String functionName
) {

  @Override
  public String toString() {
    return moduleName + "::" + typeName + "::" + functionName;
  }
}
