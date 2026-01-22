package org.enso.base_test_helpers;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Random;
import java.util.zip.CRC32;
import java.util.zip.Deflater;

public final class ImageUtils {
  private ImageUtils() {}

  private static final int RND_SEED = 42;
  private static final int CHANNELS = 4; // RGBA

  /**
   * Write a random RGBA PNG to the given path.
   *
   * @param outPath output file path (parent directories will be created if needed)
   * @param width image width (pixels)
   * @param height image height (pixels)
   * @throws IOException on IO error
   */
  public static void createRandomPng(String outPath, int width, int height) throws IOException {
    if (width <= 0 || height <= 0) {
      throw new IllegalArgumentException("width/height must be > 0");
    }

    final int rowLen = width * CHANNELS;
    byte[] pixels = new byte[width * height * CHANNELS];
    var rnd = new Random(RND_SEED);
    rnd.nextBytes(pixels);

    var out = new ByteArrayOutputStream();
    var dataOut = new DataOutputStream(out);

    // PNG signature
    dataOut.write(new byte[] {(byte) 137, 80, 78, 71, 13, 10, 26, 10});

    // IHDR chunk (13 bytes)
    var ihdr = ByteBuffer.allocate(13).order(ByteOrder.BIG_ENDIAN);
    ihdr.putInt(width);
    ihdr.putInt(height);
    ihdr.put((byte) 8); // bit depth
    ihdr.put((byte) 6); // color type 6 = truecolor + alpha (RGBA)
    ihdr.put((byte) 0); // compression
    ihdr.put((byte) 0); // filter
    ihdr.put((byte) 0); // interlace
    writeChunk(dataOut, "IHDR", ihdr.array());

    // Build raw image data: each row prefixed by filter type 0
    var raw = new ByteArrayOutputStream((rowLen + 1) * height);
    for (int y = 0; y < height; y++) {
      raw.write(0); // no filter
      int offset = y * rowLen;
      raw.write(pixels, offset, rowLen);
    }

    // Compress with zlib (Deflater)
    var deflater = new Deflater(6);
    try {
      byte[] rawBytes = raw.toByteArray();
      deflater.setInput(rawBytes);
      deflater.finish();
      ByteArrayOutputStream comp = new ByteArrayOutputStream();
      byte[] buf = new byte[4096];
      while (!deflater.finished()) {
        int len = deflater.deflate(buf);
        if (len > 0) comp.write(buf, 0, len);
      }
      deflater.end();
      writeChunk(dataOut, "IDAT", comp.toByteArray());
    } finally {
      deflater.end();
    }

    // IEND
    writeChunk(dataOut, "IEND", new byte[0]);

    dataOut.flush();

    // Ensure parent dirs and write file
    var path = Path.of(outPath);
    var parent = path.getParent();
    if (parent != null) {
      Files.createDirectories(parent);
    }
    Files.write(path, out.toByteArray());
  }

  private static void writeChunk(DataOutputStream dos, String type, byte[] data)
      throws IOException {
    byte[] typeBytes = type.getBytes(StandardCharsets.US_ASCII);
    dos.writeInt(data.length); // length (big-endian)
    dos.write(typeBytes); // chunk type
    if (data.length > 0) {
      dos.write(data); // chunk data
    }

    CRC32 crc = new CRC32();
    crc.update(typeBytes);
    if (data.length > 0) crc.update(data);
    dos.writeInt((int) crc.getValue()); // CRC (big-endian)
  }
}
