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
import java.util.stream.Collectors;
import org.enso.base.Encoding_Utils;

/**
 * A {@code Reader} which takes an {@code InputStream} and decodes it using a provided {@code
 * CharsetDecoder}.
 *
 * <p>Functionally, it should be equivalent to {@code java.io.InputStreamReader}. The major
 * difference is that this class allows more granular reporting of decoding issues - instead of just
 * replacing malformed characters with a replacement or failing at the first error, it allows to
 * both perform the replacements but also remember the positions at which the problems occurred and
 * then return a bulk report of places where the issues have been encountered.
 */
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
   * Indicates the amount of bytes consumed before the start of the current input buffer.
   *
   * <p>The input buffer is reset many times and so its position will only indicate the bytes that
   * were consumed in some current iteration, this counter allows us to compute the overall amount
   * of bytes.
   */
  private int inputBytesConsumedBeforeCurrentBuffer = 0;

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

    // If we reached end of file, we won't be able to read any more data from the input. We also ran
    // out of cached characters, so we indicate that there is no more input.
    if (eof) {
      // If the previous invocation of read set the EOF flag, it must have finished the decoding
      // process and flushed the decoder, so the input buffer must have been consumed in whole.
      assert !inputBuffer.hasRemaining();
      return -1;
    }

    // At this point we ran out of cached characters, so we will read some more input to try to get
    // new characters for the request.

    prepareOutputBuffer(len);

    int expectedInputSize = Math.max((int) (len / decoder.averageCharsPerByte()), 10);
    readInputStreamToInputBuffer(expectedInputSize);
    runDecoderOnInputBuffer();

    // We transfer as much as the user requested, anything that is remaining will be cached for the
    // next invocation.
    int toTransfer = Math.min(len, outputBuffer.remaining());
    outputBuffer.get(cbuf, off, toTransfer);
    readBytes += toTransfer;

    // If we did not read any new bytes in the call that reachedn EOF, we return EOF immediately
    // instead of postponing to the next call. Returning 0 at the end was causing division by zero
    // in the CSV parser.
    return (eof && readBytes <= 0) ? -1 : readBytes;
  }

  /**
   * Ensures that the output buffer is allocated and has enough space to fit as many characters as
   * we expect to read.
   *
   * <p>When this method is called, the output buffer should not have any remaining cached
   * characters and should be in read mode. After the method returns, the output buffer is empty and
   * left in write mode.
   */
  private void prepareOutputBuffer(int expectedCharactersCount) {
    assert outputBuffer == null || !outputBuffer.hasRemaining();
    if (outputBuffer == null || outputBuffer.capacity() < expectedCharactersCount) {
      outputBuffer = CharBuffer.allocate(expectedCharactersCount);
    } else {
      outputBuffer.clear();
    }
  }

  /**
   * Reads a chunk of data from the input stream and puts it onto the input buffer.
   *
   * <p>It updates the EOF flag if necessary.
   *
   * <p>Assumes that the input buffer is in write mode. After this function returns, the input
   * buffer is in read mode.
   */
  private void readInputStreamToInputBuffer(int expectedInputSize) throws IOException {
    int bufferedInput = inputBuffer == null ? 0 : inputBuffer.remaining();
    // We always read at least one more byte to ensure that decoding progresses.
    int bytesToRead = Math.max(expectedInputSize - bufferedInput, 1);

    ensureWorkArraySize(bytesToRead);
    int bytesActuallyRead = bufferedInputStream.read(workArray, 0, bytesToRead);
    if (bytesActuallyRead == -1) {
      eof = true;
    }

    ensureInputBufferHasEnoughFreeSpace(Math.max(0, bytesActuallyRead));

    if (bytesActuallyRead > 0) {
      inputBuffer.put(workArray, 0, bytesActuallyRead);
    }

    // We flip the input buffer back to reading mode, to be able to pass it to the decoder.
    inputBuffer.flip();
  }

  /** Allocates or grows the work array so that it can fit the amount of bytes we want to read. */
  private void ensureWorkArraySize(int bytesToRead) {
    if (workArray == null || workArray.length < bytesToRead) {
      workArray = new byte[bytesToRead];
    }
  }

  /**
   * Runs the decoder on the input buffer, transferring any decoded characters to the output buffer
   * and growing it as needed.
   *
   * <p>Even if the input buffer does not contain any remaining data, but end-of-input has been
   * encountered, one decoding step is performed to satisfy the contract of the decoder (it requires
   * one final call to the decode method signifying end of input).
   *
   * <p>After this call, the output buffer is in reading mode.
   */
  private void runDecoderOnInputBuffer() {
    while (inputBuffer.hasRemaining() || (eof && !hadEofDecodeCall)) {
      CoderResult cr = decoder.decode(inputBuffer, outputBuffer, eof);
      if (eof) {
        hadEofDecodeCall = true;
      }

      if (cr.isMalformed() || cr.isUnmappable()) {
        reportEncodingProblem();

        if (outputBuffer.remaining() < Encoding_Utils.INVALID_CHARACTER.length()) {
          growOutputBuffer();
        }
        outputBuffer.put(INVALID_CHARACTER);
        inputBuffer.position(inputBuffer.position() + cr.length());
      } else if (cr.isUnderflow()) {
        break;
      } else if (cr.isOverflow()) {
        growOutputBuffer();
      }
    }

    if (eof) {
      flushDecoder();
    }

    // After running the decoding process, we flip the output buffer into reading mode.
    outputBuffer.flip();
  }

  /** Returns the amount of bytes that have already been consumed by the decoder. */
  private int getCurrentInputPosition() {
    if (inputBuffer == null) return 0;
    return inputBytesConsumedBeforeCurrentBuffer + inputBuffer.position();
  }

  private void reportEncodingProblem() {
    encodingIssuePositions.add(getCurrentInputPosition());
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
   * <p>Assumes that the input buffer is in read mode when the method is called.
   *
   * <p>The buffer is in write mode after this call and has enough space to hold {@code bytesToRead}
   * bytes.
   */
  private void ensureInputBufferHasEnoughFreeSpace(int bytesToRead) {
    if (inputBuffer == null) {
      inputBuffer = ByteBuffer.allocate(bytesToRead);
    } else {
      int freeSpaceInInputBuffer = inputBuffer.capacity() - inputBuffer.remaining();

      // After either compacting the buffer or reallocating it, any remaining input is shifted to
      // the beginning of the buffer. Thus the bytes that preceded the current position are lost
      // (because they already have been processed), so we increase the counter to keep the global
      // position in the input.
      inputBytesConsumedBeforeCurrentBuffer += inputBuffer.position();

      if (freeSpaceInInputBuffer < bytesToRead) {
        var old = inputBuffer;
        inputBuffer = ByteBuffer.allocate(old.remaining() + bytesToRead);
        inputBuffer.put(old);
      } else {
        inputBuffer.compact();
      }
    }

    assert inputBuffer.remaining() >= bytesToRead;
  }

  /**
   * Increases the capacity of the output buffer, preserving its contents.
   *
   * <p>The buffer is assumed to be in write mode when entering this method and is left in write
   * mode when the method returns.
   */
  private void growOutputBuffer() {
    outputBuffer = Encoding_Utils.resize(outputBuffer, CharBuffer::allocate, CharBuffer::put);
  }

  @Override
  public void close() throws IOException {
    bufferedInputStream.close();
  }

  /** Returns a list of problems encountered during the decoding. */
  public List<String> getReportedProblems() {
    if (encodingIssuePositions.isEmpty()) {
      return List.of();
    } else {
      if (encodingIssuePositions.size() == 1) {
        return List.of("Encoding issues at byte " + encodingIssuePositions.get(0) + ".");
      }

      String issues =
          encodingIssuePositions.stream()
              .map(String::valueOf)
              .collect(Collectors.joining(", ", "Encoding issues at bytes ", "."));
      return List.of(issues);
    }
  }
}
