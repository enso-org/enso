package org.enso.ydoc.server.jsonrpc.model.result;

import java.util.List;
import org.enso.ydoc.server.jsonrpc.model.ContentRoot;

public record InitProtocolConnectionResult(
    String ensoVersion, String currentEdition, List<ContentRoot> contentRoots) implements Result {}
