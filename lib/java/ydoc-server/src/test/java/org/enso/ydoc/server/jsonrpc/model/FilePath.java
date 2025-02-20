package org.enso.ydoc.server.jsonrpc.model;

import java.util.List;
import java.util.UUID;

public record FilePath(UUID rootId, List<String> segments) {}
