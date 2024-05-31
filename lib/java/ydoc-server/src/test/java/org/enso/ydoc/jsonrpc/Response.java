package org.enso.ydoc.jsonrpc;

import org.enso.ydoc.jsonrpc.result.Result;

public record Response(String jsonrpc, String id, Result result) {

  public Response(String id, Result result) {
    this("2.0", id, result);
  }

  public static Response ok(String id) {
    return new Response(id, null);
  }
}
