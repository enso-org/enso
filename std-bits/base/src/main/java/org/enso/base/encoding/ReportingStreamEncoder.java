package org.enso.base.encoding;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CoderResult;

public class ReportingStreamEncoder extends Writer {
  /**
   * Creates a writer which encodes characters and writes them to the provided output stream.
   *
   * <p>The encoder reports any malformed or unmappable characters as problems and replaces them
   * with the provided replacement sequence.
   */
  public ReportingStreamEncoder(
      OutputStream outputStream, CharsetEncoder encoder, byte[] replacementSequence) {
    this.encoder = encoder;
    bufferedOutputStream = new BufferedOutputStream(outputStream);
  }

  private final BufferedOutputStream bufferedOutputStream;
  private final CharsetEncoder encoder;

  /**
   * The buffer keeping any input that has already been written but not encoded yet.
   *
   * <p>Between the calls to write, it satisfies the invariant that it is in 'reading' mode - to be
   * able to write to it, it needs to be reallocated, compacted or flipped.
   */
  private CharBuffer inputBuffer = CharBuffer.allocate(0);

  private long inputCharactersConsumedBeforeCurrentBuffer = 0;

  // TODO !
  private final byte[] replacement = new byte[0];

  private void ensureInputBufferHasEnoughFreeSpace(int bytesToAppend) {
    int freeSpaceInInputBuffer = inputBuffer.capacity() - inputBuffer.remaining();

    // After either compacting the buffer or reallocating it, any remaining input is shifted to
    // the beginning of the buffer. Thus the bytes that preceded the current position are lost
    // (because they already have been processed), so we increase the counter to keep the global
    // position in the input.
    inputCharactersConsumedBeforeCurrentBuffer += inputBuffer.position();

    if (freeSpaceInInputBuffer < bytesToAppend) {
      var old = inputBuffer;
      inputBuffer = CharBuffer.allocate(old.remaining() + bytesToAppend);
      inputBuffer.put(old);
    } else {
      inputBuffer.compact();
    }
  }

  @Override
  public void write(char[] cbuf, int off, int len) throws IOException {
    if (len < 0) {
      throw new IndexOutOfBoundsException();
    }

    ensureInputBufferHasEnoughFreeSpace(len);
    inputBuffer.put(cbuf, off, len);

    // We flip the input buffer back to reading mode, to be able to pass it to the encoder.
    inputBuffer.flip();

    runEncoderOnInputBuffer();

    // TODO write output
  }

  private void runEncoderOnInputBuffer() {
    ByteBuffer outputBuffer =
        ByteBuffer.allocate((int) (inputBuffer.remaining() * encoder.averageBytesPerChar()));
    while (inputBuffer.hasRemaining()) {
      CoderResult cr = encoder.encode(inputBuffer, outputBuffer, false);

      if (cr.isMalformed() || cr.isUnmappable()) {
        reportEncodingProblem();

        if (outputBuffer.remaining() < replacement.length) {
          growOutputBuffer();
        }

        outputBuffer.put(replacement);
        inputBuffer.position(inputBuffer.position() + cr.length());
      } else if (cr.isUnderflow()) {
        break;
      } else if (cr.isOverflow()) {
        growOutputBuffer();
      }
    }

    // TODO move this to main write
    //    bufferedOutputStream.write(output);
  }

  private void reportEncodingProblem() {
    // TODO
  }

  private void growOutputBuffer() {
    // TODO
  }

  @Override
  public void flush() throws IOException {
    // We don't flush the encoder here, because the flush operation for the encoder is supposed to
    // be run at the very end, and for a Writer the flush may be called whenever and further write
    // operations may follow it. So we do the best we can - flush the underlying stream and keep the
    // encoder intact, ready for possible writes.
    bufferedOutputStream.flush();
  }

  @Override
  public void close() throws IOException {
    bufferedOutputStream.close();
  }
}
