package org.enso.ydoc.server.jsonrpc.model;

import java.util.UUID;

public record ContentRoot(String type, UUID id, String path) {

  public ContentRoot(String type, UUID id) {
    this(type, id, null);
  }
}
