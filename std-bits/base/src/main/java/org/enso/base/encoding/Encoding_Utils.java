package org.enso.base.encoding;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;
import java.nio.charset.CodingErrorAction;
import java.util.Arrays;
import java.util.List;
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
   * Converts an array of encoded bytes into a string.
   *
   * @param bytes the bytes to convert
   * @param charset the character set to use for decoding, use {@code null} to try auto-detection
   * @return the resulting string, and any potential problems
   */
  public static WithProblems<String, DecodingProblem> from_bytes(byte[] bytes, Charset charset) {
    if (bytes == null || bytes.length == 0) {
      return new WithProblems<>("", List.of());
    }

    ByteArrayInputStream inputStream = new ByteArrayInputStream(bytes);
    ReportingStreamDecoder decoder;
    try {
      decoder = create_stream_decoder(inputStream, charset, true);
    } catch (IOException e) {
      throw new IllegalStateException("Unexpected IO exception in internal code: " + e.getMessage(), e);
    }

    CharBuffer out = CharBuffer.allocate((int) (bytes.length * decoder.averageCharsPerByte()));
    try {
      int n;
      do {
        if (!out.hasRemaining()) {
          out = resize(out, CharBuffer::allocate, CharBuffer::put);
        }
        // read is already polling safepoints so we don't have to
        n = decoder.read(out);
      } while (n > 0);
    } catch (IOException e) {
      throw new IllegalStateException("Unexpected exception: " + e.getMessage(), e);
    }

    out.flip();
    return new WithProblems<>(out.toString(), decoder.getReportedProblems());
  }

  /** Creates a new instance of {@code ReportingStreamDecoder} decoding a given charset.
   *
   * @param stream the input stream to decode
   * @param charset the character set to use for decoding, use {@code null} to try auto-detection
   * @param pollSafepoints whether to poll for safepoints during decoding.
   *                       This should be true if the decoding will run on the main thread, and false otherwise.
   */
  private static ReportingStreamDecoder create_stream_decoder(InputStream stream, Charset charset, boolean pollSafepoints) throws IOException {
    BufferedInputStream bufferedStream = new BufferedInputStream(stream);
    EncodingRepresentation representation = EncodingRepresentation.fromCharset(charset);
    // This may also advance the stream past the BOM
    Charset detectedCharset = representation.detectCharset(bufferedStream);
    CharsetDecoder decoder =
        detectedCharset
            .newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .reset();
    return new ReportingStreamDecoder(bufferedStream, decoder, pollSafepoints);
  }

  /**
   * A helper function which runs an action with a created stream decoder and closes it afterwards.
   *
   * <p>It returns the result returned from the executed action and any encoding problems that
   * occurred when processing it.
   */
  public static WithProblems<Value, DecodingProblem> with_stream_decoder(
      InputStream stream, Charset charset, Function<ReportingStreamDecoder, Value> action)
      throws IOException {
    Value result;
    ReportingStreamDecoder decoder = create_stream_decoder(stream, charset, false);
    try (decoder) {
      result = action.apply(decoder);
    }
    return new WithProblems<>(result, decoder.getReportedProblems());
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
