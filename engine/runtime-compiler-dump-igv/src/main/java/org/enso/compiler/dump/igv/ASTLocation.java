package org.enso.compiler.dump.igv;

import java.net.URI;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.dump.service.IRSource;

final class ASTLocation {
  private final IRSource<? extends IR> ctx;
  private final IdentifiedLocation loc;

  private ASTLocation(IRSource<? extends IR> ctx, IdentifiedLocation loc) {
    this.ctx = ctx;
    this.loc = loc;
  }

  public static ASTLocation fromIdentifiedLocation(
      IdentifiedLocation loc, IRSource<? extends IR> ctx) {
    return new ASTLocation(ctx, loc);
  }

  public int getLineNum() {
    return loc == null ? -1 : ctx.lineMap().apply(loc);
  }

  public int getOffsetStart() {
    return loc == null ? -1 : loc.start();
  }

  public int getOffsetEnd() {
    return loc == null ? -1 : loc.end();
  }

  public URI getLocationUri() {
    return ctx.loc();
  }
}
