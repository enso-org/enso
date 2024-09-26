package org.enso.compiler.core;

import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;

public final class EnsoParser implements AutoCloseable {
  private final Parser parser;

  public EnsoParser() {
    Parser p;
    try {
      p = Parser.create();
    } catch (LinkageError err) {
      err.printStackTrace();
      throw err;
    }
    this.parser = p;
  }

  @Override
  public synchronized void close() throws Exception {
    if (parser != null) {
      parser.close();
    }
  }

  public synchronized Module compile(CharSequence src) {
    var tree = parser.parse(src);
    return generateIR(tree);
  }

  public synchronized Tree parse(CharSequence src) {
    return parser.parse(src);
  }

  public synchronized Module generateIR(Tree t) {
    return TreeToIr.MODULE.translate(t);
  }

  public synchronized Module generateModuleIr(Tree t, Map<Location, UUID> idMap) {
    var treeToIr = new TreeToIr(idMap);
    return treeToIr.translate(t);
  }

  public synchronized scala.Option<Expression> generateIRInline(Tree t) {
    return TreeToIr.MODULE.translateInline(t);
  }
}
