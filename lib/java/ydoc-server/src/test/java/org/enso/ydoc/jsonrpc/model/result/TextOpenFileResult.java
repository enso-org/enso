package org.enso.ydoc.jsonrpc.model.result;

import org.enso.ydoc.jsonrpc.model.WriteCapability;

public record TextOpenFileResult(
    WriteCapability writeCapability, String content, String currentVersion) implements Result {}
