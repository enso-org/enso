package org.enso.ydoc.server.jsonrpc;

import com.fasterxml.jackson.databind.JsonNode;

public record JsonRpcRequest(String jsonrpc, String id, String method, JsonNode params) {}
