package org.enso.ydoc.jsonrpc.result;

import java.util.List;
import org.enso.ydoc.jsonrpc.model.ContentRoot;

public record InitProtocolConnectionResult(
    String ensoVersion, String currentEdition, List<ContentRoot> contentRoots) implements Result {}
