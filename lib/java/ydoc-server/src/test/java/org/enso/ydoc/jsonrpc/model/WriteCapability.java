package org.enso.ydoc.jsonrpc.model;

public record WriteCapability(String method, Options registerOptions) {

  public record Options(FilePath path) {}
}
