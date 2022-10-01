package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import org.enso.compiler.core.IR;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;
import org.enso.syntax2.UnsupportedSyntaxException;

public final class EnsoCompiler implements AutoCloseable {
  private final Parser parser;

  public EnsoCompiler() {
    this.parser = Parser.create();
  }

  @Override
  public void close() throws Exception {
    this.parser.close();
  }

  IR.Module compile(Source src) throws UnsupportedSyntaxException {
    var tree = parse(src);
    return generateIR(tree);
  }

  Tree parse(Source src) throws UnsupportedSyntaxException {
    return parser.parse(src.getCharacters());
  }

  public IR.Module generateIR(Tree t) {
    return TreeToIr.MODULE.translate(t);
  }
}
