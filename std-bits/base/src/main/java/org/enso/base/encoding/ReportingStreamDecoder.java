package org.enso.base.encoding;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.CharsetDecoder;
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

    // If more characters are needed, we will encode some more.
    assert !outputBuffer.hasRemaining();
    outputBuffer.reset();

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
      // TODO EOF
    }

    
    
  }


  @Override
  public void close() throws IOException {
    bufferedInputStream.close();
  }
}
