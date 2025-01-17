package org.enso.compiler.dump.igv;

import java.io.File;
import java.net.URI;
import org.enso.compiler.core.ir.IdentifiedLocation;

final class ASTLocation {
  private final int lineNum;
  // May be null
  private final URI locationUri;
  private final int offsetStart;
  private final int offsetEnd;

  private ASTLocation(int lineNum, URI locationUri, int offsetStart, int offsetEnd) {
    this.lineNum = lineNum;
    this.locationUri = locationUri;
    this.offsetStart = offsetStart;
    this.offsetEnd = offsetEnd;
  }

  public static ASTLocation fromIdentifiedLocation(IdentifiedLocation loc, File srcFile) {
    int offStart = -1;
    int offEnd = -1;
    int lineNum = 1;
    URI uri = null;
    if (loc != null) {
      offStart = loc.start();
      offEnd = loc.end();
    }
    if (srcFile != null) {
      uri = srcFile.toURI();
    }
    return new ASTLocation(lineNum, uri, offStart, offEnd);
  }

  public int getLineNum() {
    return lineNum;
  }

  public int getOffsetStart() {
    return offsetStart;
  }

  public int getOffsetEnd() {
    return offsetEnd;
  }

  public URI getLocationUri() {
    return locationUri;
  }
}
