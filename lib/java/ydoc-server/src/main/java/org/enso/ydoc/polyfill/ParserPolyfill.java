package org.enso.ydoc.polyfill;

import org.enso.syntax2.Parser;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ParserPolyfill implements AutoCloseable, ProxyExecutable, Polyfill {

  private static final Logger log = LoggerFactory.getLogger(ParserPolyfill.class);

  private static final String PARSE_TREE = "parse-tree";
  private static final String XX_HASH_128 = "xx-hash-128";
  private static final String IS_IDENT_OR_OPERATOR = "is-ident-or-operator";

  private static final String PARSER_JS = "parser.js";

  private final Parser parser;

  public ParserPolyfill() {
    Parser p;
    try {
      p = Parser.create();
    } catch (LinkageError e) {
      log.error("Failed to create parser", e);
      throw e;
    }
    this.parser = p;
  }

  @Override
  public final void initialize(Context ctx) {
    Source parserJs =
        Source.newBuilder("js", ParserPolyfill.class.getResource(PARSER_JS)).buildLiteral();

    ctx.eval(parserJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case PARSE_TREE -> {
        var input = arguments[1].asString();

        yield parser.parseInputLazy(input);
      }

      case XX_HASH_128 -> {
        var input = arguments[1].asString();

        yield Integer.toHexString(input.hashCode());
      }

      case IS_IDENT_OR_OPERATOR -> {
        var input = arguments[1].asString();

        yield parser.isIdentOrOperator(input);
      }

      default -> throw new IllegalStateException(command);
    };
  }

  @Override
  public void close() {
    parser.close();
  }
}
