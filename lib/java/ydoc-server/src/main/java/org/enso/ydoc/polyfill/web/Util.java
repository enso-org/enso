package org.enso.ydoc.polyfill.web;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.ByteSequence;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Implements the <a href="https://nodejs.org/api/util.html">Util</a> Node.js API. */
final class Util implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(Util.class);

  private static final String TEXT_DECODER_DECODE = "text-decoder-decode";
  private static final String TEXT_ENCODER_ENCODE = "text-encoder-encode";

  private static final String UTIL_JS = "util.js";

  @Override
  public void initialize(Context ctx) {
    Source jsSource = Source.newBuilder("js", getClass().getResource(UTIL_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case TEXT_DECODER_DECODE -> {
        var encoding = arguments[1].asString();
        var byteSequence = arguments[2].as(ByteSequence.class);
        var byteOffset = arguments[3].asInt();
        var byteLength = arguments[4].asInt();

        var charset = encoding == null ? StandardCharsets.UTF_8 : Charset.forName(encoding);
        var byteArray = byteSequence.subSequence(byteOffset, byteOffset + byteLength).toByteArray();

        yield charset.decode(ByteBuffer.wrap(byteArray)).toString();
      }

      case TEXT_ENCODER_ENCODE -> {
        var input = arguments[1].asString();

        yield StandardCharsets.UTF_8.encode(input);
      }

      default -> throw new IllegalStateException(command);
    };
  }
}
