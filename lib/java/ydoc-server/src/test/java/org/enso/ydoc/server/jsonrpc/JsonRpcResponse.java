package org.enso.ydoc.server.jsonrpc;

import org.enso.ydoc.server.jsonrpc.model.result.Result;

public record JsonRpcResponse(String jsonrpc, String id, Result result) {

  private static final String JSONRPC_VERSION_2_0 = "2.0";

  public JsonRpcResponse(String id, Result result) {
    this(JSONRPC_VERSION_2_0, id, result);
  }

  public static JsonRpcResponse ok(String id) {
    return new JsonRpcResponse(id, null);
  }
}
