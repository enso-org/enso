package org.enso.ydoc.server.jsonrpc.model.result;

import org.enso.ydoc.server.jsonrpc.model.WriteCapability;

public record TextOpenFileResult(
    WriteCapability writeCapability, String content, String currentVersion) implements Result {}
