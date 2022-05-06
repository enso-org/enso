package org.enso.base;

import java.io.IOException;
import java.io.InputStream;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import org.enso.base.encoding.ReportingStreamDecoder;
import org.enso.base.text.ResultWithWarnings;

public class Encoding_Utils {
  /** The replacement character used for characters that could not have been decoded. */
  public static final String INVALID_CHARACTER = "\uFFFD";

  /**
   * Converts a string into an array of bytes using the specified encoding.
   *
   * @param str the string to convert
   * @param charset the character set to use to encode the string
   * @return the UTF-8 representation of the string.
   */
  public static ResultWithWarnings<byte[]> get_bytes(String str, Charset charset) {
    if (str.isEmpty()) {
      return new ResultWithWarnings<>(new byte[0]);
    }

    CharsetEncoder encoder =
        charset
            .newEncoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .reset();

    CharBuffer in = CharBuffer.wrap(str.toCharArray());
    ByteBuffer out = ByteBuffer.allocate((int) (in.remaining() * encoder.averageBytesPerChar()));

    StringBuilder warnings = null;
    while (in.hasRemaining()) {
      CoderResult cr = encoder.encode(in, out, true);
      if (cr.isMalformed() || cr.isUnmappable()) {
        // Get current position for error reporting
        int position = in.position();

        if (out.remaining() < encoder.replacement().length) {
          out = resize(out, ByteBuffer::allocate, ByteBuffer::put);
        }
        out.put(encoder.replacement());
        in.position(in.position() + cr.length());

        if (warnings == null) {
          warnings = new StringBuilder();
          warnings.append("Encoding issues at ");
        } else {
          warnings.append(", ");
        }
        warnings.append(position);
      } else if (cr.isUnderflow()) {
        // Finished
        while (encoder.flush(out) == CoderResult.OVERFLOW) {
          out = resize(out, ByteBuffer::allocate, ByteBuffer::put);
        }
        break;
      } else if (cr.isOverflow()) {
        out = resize(out, ByteBuffer::allocate, ByteBuffer::put);
      }
    }

    out.flip();
    byte[] array = out.array();
    if (out.limit() != array.length) {
      array = Arrays.copyOf(array, out.limit());
    }

    if (warnings == null) {
      return new ResultWithWarnings<>(array);
    }

    warnings.append(".");
    return new ResultWithWarnings<>(array, warnings.toString());
  }

  private static <T extends Buffer> T resize(T old, IntFunction<T> allocate, BiConsumer<T, T> put) {
    int n = old.capacity();
    int new_n = 2 * n + 1;
    T o = allocate.apply(new_n);
    old.flip();
    put.accept(o, old);
    return o;
  }

  /**
   * Converts an array of encoded bytes into a string.
   *
   * @param bytes the bytes to convert
   * @param charset the character set to use to decode the bytes
   * @return the resulting string
   */
  public static ResultWithWarnings<String> from_bytes(byte[] bytes, Charset charset) {
    if (bytes.length == 0) {
      return new ResultWithWarnings<>("");
    }

    CharsetDecoder decoder =
        charset
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .reset();

    ByteBuffer in = ByteBuffer.wrap(bytes);
    CharBuffer out = CharBuffer.allocate((int) (bytes.length * decoder.averageCharsPerByte()));

    StringBuilder warnings = null;
    while (in.hasRemaining()) {
      CoderResult cr = decoder.decode(in, out, true);
      if (cr.isMalformed() || cr.isUnmappable()) {
        // Get current position for error reporting
        int position = in.position();

        if (out.remaining() < INVALID_CHARACTER.length()) {
          out = resize(out, CharBuffer::allocate, CharBuffer::put);
        }
        out.put(INVALID_CHARACTER);
        in.position(in.position() + cr.length());

        if (warnings == null) {
          warnings = new StringBuilder();
          warnings.append("Encoding issues at ");
        } else {
          warnings.append(", ");
        }
        warnings.append(position);
      } else if (cr.isUnderflow()) {
        // Finished
        while (decoder.flush(out) == CoderResult.OVERFLOW) {
          out = resize(out, CharBuffer::allocate, CharBuffer::put);
        }
        break;
      } else if (cr.isOverflow()) {
        out = resize(out, CharBuffer::allocate, CharBuffer::put);
      }
    }

    out.flip();

    if (warnings == null) {
      return new ResultWithWarnings<>(out.toString());
    }

    warnings.append(".");
    return new ResultWithWarnings<>(out.toString(), warnings.toString());
  }

  private static ReportingStreamDecoder create_stream_decoder(InputStream stream, Charset charset) {
    CharsetDecoder decoder =
        charset
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .reset();
    return new ReportingStreamDecoder(stream, decoder);
  }

  public static <R> R with_stream_decoder(
      InputStream stream, Charset charset, Function<ReportingStreamDecoder, R> action)
      throws IOException {
    try (ReportingStreamDecoder decoder = create_stream_decoder(stream, charset)) {
      return action.apply(decoder);
    }
  }
}
