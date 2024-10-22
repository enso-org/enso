package org.enso.compiler.core;

import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.syntax2.Parser;
import scala.Option;

public final class EnsoParser {
  private EnsoParser() {}

  public static Module compile(CharSequence src) {
    return compile(src, null);
  }

  public static Module compile(CharSequence src, Map<Location, UUID> idMap) {
    var tree = Parser.parseModule(src);
    var treeToIr = TreeToIr.MODULE;
    if (idMap != null) {
      treeToIr = new TreeToIr(idMap);
    }
    return treeToIr.translate(tree);
  }

  public static Expression.Block compileBlock(CharSequence src) {
    var tree = Parser.parseBlock(src);
    return TreeToIr.MODULE.translateBlock(tree);
  }

  public static Option<Expression> compileInline(CharSequence src) {
    var tree = Parser.parseBlock(src);
    return TreeToIr.MODULE.translateInline(tree);
  }

  /**
   * Free retained state of all parsers. Parser buffers are retained per-thread for reuse; this
   * function drops those reusable buffers. If the parser is used again after this call, it will
   * allocate new buffers as needed.
   */
  public static void freeAll() {
    Parser.freeAll();
  }
}
