package org.enso.ydoc.jsonrpc.model.result;

import java.util.List;
import org.enso.ydoc.jsonrpc.model.FileSystemObject;

public record FileListResult(List<FileSystemObject> paths) implements Result {}
