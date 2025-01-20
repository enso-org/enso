package org.enso.ydoc.server.jsonrpc.model.result;

import java.util.List;
import org.enso.ydoc.server.jsonrpc.model.FileSystemObject;

public record FileListResult(List<FileSystemObject> paths) implements Result {}
