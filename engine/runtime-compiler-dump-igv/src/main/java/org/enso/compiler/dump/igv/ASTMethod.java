package org.enso.compiler.dump.igv;

final class ASTMethod {
  static final ASTMethod UNKNOWN = new ASTMethod();

  public String getName() {
    return "<unknown>";
  }

  static final class Signature {
    static final Signature NONE = new Signature();

    private Signature() {}
  }
}
