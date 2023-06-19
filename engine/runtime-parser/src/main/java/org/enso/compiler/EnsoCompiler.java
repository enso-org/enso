package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import org.enso.compiler.core.IR;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;

public final class EnsoCompiler implements AutoCloseable {
  private final Parser parser;

  public EnsoCompiler() {
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

  public IR.Module compile(CharSequence src) {
    var tree = parser.parse(src);
    return generateIR(tree);
  }

  IR.Module compile(Source src) {
    var tree = parse(src);
    return generateIR(tree);
  }

  public Tree parse(Source src) {
    return parser.parse(src.getCharacters());
  }

  public Tree parse(CharSequence src) {
    return parser.parse(src);
  }

  public IR.Module generateIR(Tree t) {
    return TreeToIr.MODULE.translate(t);
  }

  public scala.Option<IR.Expression> generateIRInline(Tree t) {
    return TreeToIr.MODULE.translateInline(t);
  }
}
