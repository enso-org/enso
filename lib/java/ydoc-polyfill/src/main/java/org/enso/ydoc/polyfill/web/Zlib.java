package org.enso.ydoc.polyfill.web;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.function.Function;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterOutputStream;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.ByteSequence;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Implements the <a href="https://nodejs.org/api/zlib.html">Zlib</a> Node.js interface. */
final class Zlib implements ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(Zlib.class);

  private static final String BUFFER_FROM = "buffer-from";
  private static final String BUFFER_TO_STRING = "buffer-to-string";
  private static final String ENCODING_BASE64 = "base64";
  private static final String ENCODING_BASE64_URL = "base64url";

  private static final String ZLIB_DEFLATE_SYNC = "zlib-deflate-sync";
  private static final String ZLIB_INFLATE_SYNC = "zlib-inflate-sync";

  private static final String ZLIB_JS = "zlib.js";

  final void initialize(Function<java.net.URL, Value> eval) {
    var fn = eval.apply(getClass().getResource(ZLIB_JS));
    fn.execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    final var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case BUFFER_FROM -> {
        final var text = arguments[1].asString();
        final var encoding = arguments[2].asString();

        yield switch (encoding) {
          case ENCODING_BASE64 -> {
            final var buffer = StandardCharsets.UTF_8.encode(text);
            yield Base64.getDecoder().decode(buffer);
          }
          case ENCODING_BASE64_URL -> {
            final var buffer = StandardCharsets.UTF_8.encode(text);
            yield Base64.getUrlDecoder().decode(buffer);
          }
          case null -> StandardCharsets.UTF_8.encode(text);
          default -> {
            Charset charset;
            try {
              charset = Charset.forName(encoding);
            } catch (IllegalArgumentException e) {
              throw new RuntimeException("Unknown encoding: " + encoding, e);
            }
            yield charset.encode(text);
          }
        };
      }

      case BUFFER_TO_STRING -> {
        final var byteSequence = arguments[1].as(ByteSequence.class);
        final var encoding = arguments[2].asString();

        yield switch (encoding) {
          case ENCODING_BASE64 -> {
            final var arr = Base64.getEncoder().encode(byteSequence.toByteArray());
            yield new String(arr, StandardCharsets.UTF_8);
          }
          case ENCODING_BASE64_URL -> {
            final var arr = Base64.getUrlEncoder().encode(byteSequence.toByteArray());
            yield new String(arr, StandardCharsets.UTF_8);
          }
          case null -> {
            final var buffer = ByteBuffer.wrap(byteSequence.toByteArray());
            yield StandardCharsets.UTF_8.decode(buffer).toString();
          }
          default -> {
            Charset charset;
            try {
              charset = Charset.forName(encoding);
            } catch (IllegalArgumentException e) {
              throw new RuntimeException("Unknown encoding: " + encoding, e);
            }
            final var buffer = ByteBuffer.wrap(byteSequence.toByteArray());
            yield charset.decode(buffer).toString();
          }
        };
      }

      case ZLIB_DEFLATE_SYNC -> {
        final var byteSequence = arguments[1].as(ByteSequence.class);

        final var output = new ByteArrayOutputStream();
        try (final var deflater = new DeflaterOutputStream(output)) {
          deflater.write(byteSequence.toByteArray());
        } catch (IOException e) {
          throw new RuntimeException("Failed to deflate.", e);
        }

        yield ByteBuffer.wrap(output.toByteArray());
      }

      case ZLIB_INFLATE_SYNC -> {
        final var byteSequence = arguments[1].as(ByteSequence.class);

        final var output = new ByteArrayOutputStream();
        try (final var inflater = new InflaterOutputStream(output)) {
          inflater.write(byteSequence.toByteArray());
        } catch (IOException e) {
          throw new RuntimeException("Failed to inflate.", e);
        }

        yield ByteBuffer.wrap(output.toByteArray());
      }

      default -> throw new IllegalStateException(command);
    };
  }
}
