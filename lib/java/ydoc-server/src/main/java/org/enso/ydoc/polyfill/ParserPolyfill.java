package org.enso.ydoc.polyfill;

import org.enso.syntax2.Parser;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ParserPolyfill implements ProxyExecutable, Polyfill {

  private static final Logger log = LoggerFactory.getLogger(ParserPolyfill.class);

  private static final String PARSE_BLOCK = "parse-block";
  private static final String PARSE_MODULE = "parse-module";
  private static final String XX_HASH_128 = "xx-hash-128";
  private static final String IS_IDENT_OR_OPERATOR = "is-ident-or-operator";

  private static final String PARSER_JS = "parser.js";

  public ParserPolyfill() {}

  @Override
  public void initialize(Context ctx) {
    Source parserJs =
        Source.newBuilder("js", ParserPolyfill.class.getResource(PARSER_JS)).buildLiteral();

    ctx.eval(parserJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case PARSE_BLOCK -> {
        var input = arguments[1].asString();

        yield Parser.parseBlockLazy(input);
      }

      case PARSE_MODULE -> {
        var input = arguments[1].asString();

        yield Parser.parseModuleLazy(input);
      }

      case XX_HASH_128 -> {
        var input = arguments[1].asString();

        yield Integer.toHexString(input.hashCode());
      }

      case IS_IDENT_OR_OPERATOR -> {
        var input = arguments[1].asString();

        yield Parser.isIdentOrOperator(input);
      }

      default -> throw new IllegalStateException(command);
    };
  }
}
