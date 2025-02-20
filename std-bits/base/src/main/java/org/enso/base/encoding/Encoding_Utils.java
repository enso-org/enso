package org.enso.base.encoding;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.util.Arrays;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntFunction;
import org.enso.base.WithProblems;
import org.enso.base.text.ResultWithWarnings;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

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

    Context context = Context.getCurrent();
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

      context.safepoint();
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

  /**
   * Checks if the following stream can be decoded without errors using the given charset.
   *
   * <p>The stream is (partially or wholly) consumed as part of this process.
   */
  public static boolean canDecodeWithoutErrors(InputStream stream, Charset charset)
      throws IOException {
    DecodingProblemAggregator problemAggregator = new DecodingProblemAggregator();
    try (var decoder = new ReportingStreamDecoder(stream, charset, problemAggregator, true)) {
      char[] tmpBuffer = new char[1024];
      while (decoder.read(tmpBuffer) >= 0) {
        if (problemAggregator.hasEncounteredInvalidCharacters()) {
          // early exit - no need to process the stream any further
          return false;
        }
      }
    }

    // Check one more time after EOF
    return !problemAggregator.hasEncounteredInvalidCharacters();
  }

  /** Creates a new instance of {@code ReportingStreamEncoder} encoding a given charset. */
  private static ReportingStreamEncoder create_stream_encoder(
      OutputStream stream, Charset charset, byte[] replacementSequence) {
    CharsetEncoder encoder =
        charset
            .newEncoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .reset();
    return new ReportingStreamEncoder(stream, encoder, replacementSequence);
  }

  /**
   * A helper function which runs an action with a created stream encoder and closes it afterwards.
   *
   * <p>It returns the result returned from the executed action and any encoding problems that
   * occurred when processing it.
   */
  public static WithProblems<Value, String> with_stream_encoder(
      OutputStream stream,
      Charset charset,
      byte[] replacementSequence,
      Function<ReportingStreamEncoder, Value> action)
      throws IOException {
    Value result;
    ReportingStreamEncoder encoder = create_stream_encoder(stream, charset, replacementSequence);
    try (encoder) {
      result = action.apply(encoder);
    }
    return new WithProblems<>(result, encoder.getReportedProblems());
  }

  /**
   * A generic function to resize a buffer.
   *
   * @param <T> the type of the buffer to allocate
   * @param old the buffer to resize
   * @param allocate a function allocating a buffer of required type of a given size
   * @param put a function which can transfer data from the old buffer into the new one
   * @return the new buffer with increased capacity
   */
  public static <T extends Buffer> T resize(T old, IntFunction<T> allocate, BiConsumer<T, T> put) {
    int n = old.capacity();
    int new_n = (3 * n) / 2 + 1;
    T o = allocate.apply(new_n);
    old.flip();
    put.accept(o, old);
    return o;
  }
}
