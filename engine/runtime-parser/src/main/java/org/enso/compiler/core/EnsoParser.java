package org.enso.compiler.core;

import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.syntax2.Parser;

public final class EnsoParser {
  public static Module compile(CharSequence src) {
    return compile(src, null);
  }

  public static Module compile(CharSequence src, Map<Location, UUID> idMap) {
    var tree = Parser.parse(src);
    var treeToIr = TreeToIr.MODULE;
    if (idMap != null) {
      treeToIr = new TreeToIr(idMap);
    }
    return treeToIr.translate(tree);
  }

  public static scala.Option<Expression> compileInline(CharSequence src) {
    var tree = Parser.parse(src);
    return TreeToIr.MODULE.translateInline(tree);
  }
}
