package org.enso.base.encoding;

import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public abstract sealed class EncodingRepresentation {
  private static final byte[] UTF_8_BOM = new byte[] {(byte) 0xEF, (byte) 0xBB, (byte) 0xBF};
  private static final byte[] UTF_16_BE_BOM = new byte[] {(byte) 0xFE, (byte) 0xFF};
  private static final byte[] UTF_16_LE_BOM = new byte[] {(byte) 0xFF, (byte) 0xFE};

  private static byte[] peekStream(BufferedInputStream stream, int n) throws IOException {
    byte[] buffer = new byte[n];
    stream.mark(n + 1);
    int offset = 0;
    while (offset < n) {
      int read = stream.read(buffer, offset, n - offset);
      if (read == -1) {
        break;
      }
      offset += read;
    }
    stream.reset();
    return buffer;
  }

  private static void skipStream(BufferedInputStream stream, int n) throws IOException {
    try {
      stream.skipNBytes(n);
    } catch (EOFException eofException) {
      // ignore early EOF
    }
  }

  private static boolean startsWith(byte[] buffer, byte[] prefix) {
    if (buffer.length < prefix.length) {
      return false;
    }

    for (int i = 0; i < prefix.length; i++) {
      if (buffer[i] != prefix[i]) {
        return false;
      }
    }
    return true;
  }

  public static EncodingRepresentation fromCharset(Charset charset) {
    if (charset == null) {
      return new Default();
    } else if (UnicodeHandlingBOM.isSupported(charset)) {
      return UnicodeHandlingBOM.fromUnicodeCharset(charset);
    } else {
      return new Other(charset);
    }
  }

  /**
   * Detects the effective charset based on initial data present in the stream.
   *
   * @param stream the stream to detect the charset for. Initial BOM header may be deliberately
   *     consumed by this method, so that it is ignored for further processing. No other data is
   *     consumed (we only peek).
   * @param problemAggregator an aggregator to report any warnings about detected encoding, or
   *     report context metadata to be used in future errors reported by other components.
   */
  public abstract Charset detectCharset(
      BufferedInputStream stream, DecodingProblemAggregator problemAggregator) throws IOException;

  private static final class Default extends EncodingRepresentation {
    // Note: the Windows-1252 fallback is not implemented as part of this detection - it only allows
    // to distinguish UTF BOMs.
    @Override
    public Charset detectCharset(
        BufferedInputStream stream, DecodingProblemAggregator problemAggregator)
        throws IOException {
      byte[] beginning = peekStream(stream, 3);
      if (startsWith(beginning, UTF_8_BOM)) {
        skipStream(stream, UTF_8_BOM.length);
        notifyContextAboutAssumedEncoding(problemAggregator, "UTF-8");
        return StandardCharsets.UTF_8;
      } else if (startsWith(beginning, UTF_16_BE_BOM)) {
        skipStream(stream, UTF_16_BE_BOM.length);
        notifyContextAboutAssumedEncoding(problemAggregator, "UTF-16 BE");
        return StandardCharsets.UTF_16BE;
      } else if (startsWith(beginning, UTF_16_LE_BOM)) {
        skipStream(stream, UTF_16_LE_BOM.length);
        notifyContextAboutAssumedEncoding(problemAggregator, "UTF-16 LE");
        return StandardCharsets.UTF_16LE;
      } else {
        // If no BOM we fallback to UTF-8.
        return StandardCharsets.UTF_8;
      }
    }

    private static void notifyContextAboutAssumedEncoding(DecodingProblemAggregator problemAggregator, String encodingName) {
      String prefix = "An " + encodingName + " BOM was detected, so " + encodingName + " encoding has been assumed, but some characters seem invalid: ";
      problemAggregator.setInvalidCharacterErrorPrefix(prefix);
    }
  }

  private static final class Other extends EncodingRepresentation {
    private final Charset charset;

    public Other(Charset charset) {
      this.charset = charset;
    }

    @Override
    public Charset detectCharset(
        BufferedInputStream stream, DecodingProblemAggregator problemAggregator)
        throws IOException {
      // We ignore the stream as we just use the provided encoding as-is.
      return charset;
    }
  }

  private static final class UnicodeHandlingBOM extends EncodingRepresentation {
    private final Charset charset;
    private final byte[] expectedBOM;

    // TODO unexpected BOM

    private UnicodeHandlingBOM(Charset charset, byte[] expectedBOM) {
      this.charset = charset;
      this.expectedBOM = expectedBOM;
    }

    private static boolean isSupported(Charset charset) {
      return charset.equals(StandardCharsets.UTF_8)
          || charset.equals(StandardCharsets.UTF_16BE)
          || charset.equals(StandardCharsets.UTF_16LE);
    }

    private static UnicodeHandlingBOM fromUnicodeCharset(Charset charset) {
      if (charset.equals(StandardCharsets.UTF_8)) {
        return new UnicodeHandlingBOM(charset, UTF_8_BOM);
      } else if (charset.equals(StandardCharsets.UTF_16BE)) {
        return new UnicodeHandlingBOM(charset, UTF_16_BE_BOM);
      } else if (charset.equals(StandardCharsets.UTF_16LE)) {
        return new UnicodeHandlingBOM(charset, UTF_16_LE_BOM);
      } else {
        throw new IllegalArgumentException("Not a supported Unicode charset: " + charset);
      }
    }

    @Override
    public Charset detectCharset(
        BufferedInputStream stream, DecodingProblemAggregator problemAggregator)
        throws IOException {
      byte[] beginning = peekStream(stream, expectedBOM.length);
      if (startsWith(beginning, expectedBOM)) {
        skipStream(stream, expectedBOM.length);
      } else {
        // TODO check for unexpected BOMs
      }
      return charset;
    }
  }
}
