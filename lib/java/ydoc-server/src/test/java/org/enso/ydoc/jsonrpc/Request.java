package org.enso.ydoc.jsonrpc;

import com.fasterxml.jackson.databind.JsonNode;

public record Request(String jsonrpc, String id, String method, JsonNode params) {}
