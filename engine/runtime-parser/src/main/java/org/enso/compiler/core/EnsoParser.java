package org.enso.compiler.core;

import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;

import java.util.Map;
import java.util.UUID;

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
  public void close() throws Exception {
    if (parser != null) {
      parser.close();
    }
  }

  public Module compile(CharSequence src) {
    var tree = parser.parse(src);
    return generateIR(tree);
  }

  public Tree parse(CharSequence src) {
    return parser.parse(src);
  }

  public Module generateIR(Tree t) {
    return TreeToIr.MODULE.translate(t);
  }

  public Module generateModuleIr(Tree t, Map<Location, UUID> idMap) {
    var treeToIr = new TreeToIr(idMap);
    return treeToIr.translate(t);
  }

  public scala.Option<Expression> generateIRInline(Tree t) {
    return TreeToIr.MODULE.translateInline(t);
  }
}
