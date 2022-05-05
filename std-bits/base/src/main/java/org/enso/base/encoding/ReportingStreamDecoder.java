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
    outputBuffer = CharBuffer.allocate((int) (10 * decoder.averageCharsPerByte()));
  }

  private final BufferedInputStream bufferedInputStream;
  private final CharsetDecoder decoder;
  private CharBuffer outputBuffer;
  private ByteBuffer inputBuffer = null;
  private byte[] workArray = null;
  private boolean eof = false;
  List<Integer> encodingIssuePositions = new ArrayList<>();

  @Override
  public int read(char[] cbuf, int off, int len) throws IOException {
    if (len == 0) return 0;
    int readBytes = 0;

    // First feed the pending characters that were already decoded.
    if (outputBuffer.hasRemaining()) {
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

    // If we reached end of file, we won't be able to ready any more data from the input.
    if (eof) return -1;

    // If more characters are needed, we will encode some more.
    assert !outputBuffer.hasRemaining();
    outputBuffer.clear();

    // Fill-up the input buffer reading from the input.
    int expectedInputSize = Math.max((int) (len / decoder.averageCharsPerByte()), 10);
    if (inputBuffer == null || inputBuffer.remaining() < expectedInputSize) {
      if (inputBuffer == null || inputBuffer.capacity() < expectedInputSize) {
        var old = inputBuffer;
        inputBuffer = ByteBuffer.allocate(expectedInputSize);
        if (old != null) {
          old.flip();
          inputBuffer.put(old);
        }
      } else {
        inputBuffer.compact();
      }
    }
    
    int bytesToRead = expectedInputSize - inputBuffer.position();
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

    inputBuffer.flip();


    while (inputBuffer.hasRemaining()) {
      CoderResult cr = decoder.decode(inputBuffer, outputBuffer, eof);
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

    outputBuffer.flip();
    int toTransfer = Math.min(len, outputBuffer.remaining());
    outputBuffer.get(cbuf, off, toTransfer);
    readBytes += toTransfer;

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
