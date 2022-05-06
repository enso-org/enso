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

  /**
   * We re-use the work array between calls to read, to avoid re-allocating it on each call. It is
   * only re-allocated if it needs to be bigger than before.
   */
  private byte[] workArray = null;

  /**
   * A flag that is set once the end of input has been reached.
   *
   * <p>This informs us that no more input will be available from the input stream. There may still
   * be some pending characters in the output buffer.
   */
  private boolean eof = false;

  /**
   * Specifies if the last {@code decoder.decode} call with the {@code endOfInput = true} argument
   * has been made.
   */
  private boolean hadEofDecodeCall = false;

  /**
   * A list of positions containing encoding issues like malformed characters.
   *
   * <p>Used for reporting warnings.
   */
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
      // If  the previous invocation of read set the EOF flag, it must have finished the decoding
      // process and flushed the decoder, so the input buffer must have been consumed in whole.
      assert !inputBuffer.hasRemaining();
      return -1;
    }

    // If more characters are needed, we will encode some more. All cached output has been consumed,
    // so we 'reset' the output buffer to be ready for new data. It is now in writing mode.
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
    ensureInputBufferHasEnoughFreeSpace(bytesToRead);

    // Invariant: at this point, input buffer is in write mode and has enough space to fit {@code
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

    // We flip the input buffer back to reading mode, to be able to pass it to the decoder.
    inputBuffer.flip();

    runDecoderOnInputBuffer();
    if (eof) {
      flushDecoder();
    }

    // After writing the decoded characters, we flip the output buffer into reading mode.
    outputBuffer.flip();

    // We transfer as much as the user requested, anything that is remaining will be cached for the
    // next invocation.
    int toTransfer = Math.min(len, outputBuffer.remaining());
    outputBuffer.get(cbuf, off, toTransfer);
    readBytes += toTransfer;

    // If we did not read any new bytes in the call that reachedn EOF, we return EOF immediately
    // instead of postponing to the next call. Returning 0 at the end was causing division by zero
    // in the CSV parser.
    if (eof && readBytes <= 0) return -1;
    return readBytes;
  }

  /**
   * Runs the decoder on the input buffer, transferring any decoded characters to the output buffer
   * and growing it as needed.
   *
   * <p>Even if the input buffer does not contain any remaining data, but end-of-input has been
   * encountered, one decoding step is performed to satisfy the contract of the decoder (it requires
   * one final call to the decode method signifying end of input).
   */
  private void runDecoderOnInputBuffer() {
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
  }

  /**
   * Flushes the decoder, growing the buffer as needed to ensure that any additional output from the
   * decoder fits.
   */
  private void flushDecoder() {
    while (decoder.flush(outputBuffer) == CoderResult.OVERFLOW) {
      growOutputBuffer();
    }
  }

  /**
   * Ensures that the input buffer has enough free space to hold the number of bytes that we want to
   * read.
   *
   * <p>If necessary, the buffer is allocated or grown, preserving any existing content.
   *
   * <p>The buffer is in write mode after this call.
   */
  private void ensureInputBufferHasEnoughFreeSpace(int bytesToRead) {
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
  }

  /**
   * Increases the capacity of the output buffer, preserving its contents.
   *
   * <p>The buffer is assumed to be in write mode when entering this method and is left in write
   * mode when the method returns.
   */
  private void growOutputBuffer() {
    int newSize = ((3 * outputBuffer.capacity()) / 2) + 1;
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
