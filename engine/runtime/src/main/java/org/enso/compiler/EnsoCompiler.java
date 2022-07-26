package org.enso.compiler;

import com.oracle.truffle.api.source.Source;
import org.enso.compiler.core.IR;
import org.enso.syntax2.Parser;
import org.enso.syntax2.Tree;

final class EnsoCompiler implements AutoCloseable {
    private final Parser parser;

  EnsoCompiler() {
    this.parser = Parser.create();
  }

  @Override
  public void close() throws Exception {
    this.parser.close();
  }
  
  Tree parse(Source src) throws Parser.UnsupportedSyntaxException {
      return parser.parse(src.getCharacters());
  }
  
  IR generateIR(Tree t) {
    System.out.println(t);
    return null;
  }
}
