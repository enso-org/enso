package org.enso.base.encoding;

import static org.enso.base.Encoding_Utils.INVALID_CHARACTER;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CoderResult;
import java.util.ArrayList;
import java.util.List;
import org.enso.base.Encoding_Utils;

public class ReportingStreamDecoder extends Reader {
  public ReportingStreamDecoder(InputStream stream, CharsetDecoder decoder) {
    bufferedInputStream = new BufferedInputStream(stream);
    this.decoder = decoder;
  }

  private final BufferedInputStream bufferedInputStream;
  private final CharsetDecoder decoder;

  /**
   * The buffer keeping any characters that have already been decoded, but not consumed by the user
   * yet.
   *
   * <p>Between the calls to read, it satisfies the invariant that it is in 'reading' mode.
   */
  private CharBuffer outputBuffer = null;

  /**
   * The buffer keeping any input that has already been read but not decoded yet.
   *
   * <p>Between the calls to read, it satisfies the invariant that it is in 'reading' mode - to be
   * able to write to it, it needs to be reallocated, compacted or flipped.
   */
  private ByteBuffer inputBuffer = null;

  private byte[] workArray = null;
  private boolean eof = false;

  /**
   * Specifies if the last {@code decoder.decode} call with the {@code endOfInput = true} argument
   * has been made.
   */
  private boolean hadEofDecodeCall = false;

  List<Integer> encodingIssuePositions = new ArrayList<>();

  @Override
  public int read(char[] cbuf, int off, int len) throws IOException {
    if (len == 0) return 0;
    int readBytes = 0;

    // First feed the pending characters that were already decoded.
    if (outputBuffer != null && outputBuffer.hasRemaining()) {
      int toTransfer = Math.min(len, outputBuffer.remaining());
      outputBuffer.get(cbuf, off, toTransfer);
      off += toTransfer;
      len -= toTransfer;
      readBytes += toTransfer;
    }

    // If the request is satisfied, we do not continue.
    if (len <= 0) {
      return readBytes;
    }

    // If we reached end of file, we won't be able to read any more data from the input.
    if (eof) {
      return -1;
    }

    // If more characters are needed, we will encode some more. All cached output has been consumed,
    // so we 'reset' the outputBuffer to be ready for new data. It is now in writing mode.
    if (outputBuffer == null) {
      outputBuffer = CharBuffer.allocate(len);
    } else {
      assert !outputBuffer.hasRemaining();
      outputBuffer.clear();
    }

    // Fill-up the input buffer reading from the input.
    int expectedInputSize = Math.max((int) (len / decoder.averageCharsPerByte()), 10);
    int bufferedInput = inputBuffer == null ? 0 : inputBuffer.remaining();

    // We always read at least one more byte to ensure that decoding progresses.
    int bytesToRead = Math.max(expectedInputSize - bufferedInput, 1);

    if (inputBuffer == null) {
      inputBuffer = ByteBuffer.allocate(bytesToRead);
    } else {
      int freeSpaceInInputBuffer = inputBuffer.capacity() - inputBuffer.remaining();
      if (freeSpaceInInputBuffer < bytesToRead) {
        var old = inputBuffer;
        old.flip();
        inputBuffer = ByteBuffer.allocate(old.remaining() + bytesToRead);
        inputBuffer.put(old);
      } else {
        inputBuffer.compact();
        assert inputBuffer.remaining() >= bytesToRead;
      }
    }

    // Invariant: at this point, inputBuffer is in write mode and has enough space to fit {@code
    // bytesToRead} bytes.
    if (workArray == null || workArray.length < bytesToRead) {
      workArray = new byte[bytesToRead];
    }

    int bytesActuallyRead = bufferedInputStream.read(workArray, 0, bytesToRead);
    if (bytesActuallyRead == -1) {
      eof = true;
    }

    if (bytesActuallyRead > 0) {
      inputBuffer.put(workArray, 0, bytesActuallyRead);
    }

    // We flip the inputBuffer back to reading mode, to be able to pass it to the decoder.
    inputBuffer.flip();

    // We run the decoding as long as our buffered input is available. Even if there is no more
    // input available, but we encountered end-of-file, we do one more call to let the decoder know
    // that we ran out of input, as required by its contract. This may happen if in a previous
    // invocation we read all the remaining input and in the current invocation we immediately reach
    // EOF - thus the input buffer will be empty and normally we wouldn't run the decoder at all.
    while (inputBuffer.hasRemaining() || (eof && !hadEofDecodeCall)) {
      CoderResult cr = decoder.decode(inputBuffer, outputBuffer, eof);
      if (eof) {
        hadEofDecodeCall = true;
      }

      if (cr.isMalformed() || cr.isUnmappable()) {
        // Get current position for error reporting
        int position = 0; // TODO

        if (outputBuffer.remaining() < Encoding_Utils.INVALID_CHARACTER.length()) {
          growOutputBuffer();
        }
        outputBuffer.put(INVALID_CHARACTER);
        inputBuffer.position(inputBuffer.position() + cr.length());

        encodingIssuePositions.add(position);
      } else if (cr.isUnderflow()) {
        break;
      } else if (cr.isOverflow()) {
        growOutputBuffer();
      }
    }

    if (eof) {
      while (decoder.flush(outputBuffer) == CoderResult.OVERFLOW) {
        growOutputBuffer();
      }
    }

    // After writing the decoded characters, we flip the buffer into reading mode.
    outputBuffer.flip();

    // We transfer as much as the user requested, anything that is remaining will be cached for the
    // next invocation.
    int toTransfer = Math.min(len, outputBuffer.remaining());
    outputBuffer.get(cbuf, off, toTransfer);
    readBytes += toTransfer;

    if (eof && readBytes <= 0) return -1;
    return readBytes;
  }

  private void growOutputBuffer() {
    int newSize = 2 * outputBuffer.capacity() + 1;
    CharBuffer newBuffer = CharBuffer.allocate(newSize);
    outputBuffer.flip();
    newBuffer.put(outputBuffer);
    outputBuffer = newBuffer;
  }

  @Override
  public void close() throws IOException {
    bufferedInputStream.close();
  }

  public List<String> getReportedProblems() {
    if (encodingIssuePositions.isEmpty()) {
      return List.of();
    } else {
      return List.of("Encoding issues at TODO");
    }
  }
}
