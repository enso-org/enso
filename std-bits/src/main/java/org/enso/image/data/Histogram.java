package org.enso.image.data;

import org.opencv.core.Mat;

public class Histogram {

  public static final int BINS = 128;

  private final long[] data;
  private final int channels;

  public Histogram(long[] data, int channels) {
    this.data = data;
    this.channels = channels;
  }

  public static Histogram create(Mat image) {
    byte[] data = new byte[(int) image.total() * image.channels()];
    image.get(0, 0, data);
    long[] hist = new long[BINS * image.channels()];
    int divisor = getDivisor(image.elemSize1());
    int offset = getOffset(image.elemSize1());

    for (int y = 0; y < image.rows(); y++) {
      for (int x = 0; x < image.cols(); x++) {
        for (int c = 0; c < image.channels(); c++) {
          double pixelValue = data[(y * image.cols() + x) * image.channels() + c];
          hist[((int) pixelValue + offset) / divisor * image.channels() + c] += 1;
        }
      }
    }

    return new Histogram(hist, image.channels());
  }

  public int get_channels() {
    return channels;
  }

  public long[] get_data() {
    return data;
  }

  public long[] get_data(int channel) {
    return get_channel_data(data, channels, channel);
  }

  public static long[] get_channel_data(long[] hist, int channels, int index) {
    if (index >= channels) {
      return new long[0];
    }
    long[] channel = new long[BINS];
    for (int i = 0; i < BINS; i++) {
      channel[i] = hist[i * channels + index];
    }
    return channel;
  }

  private static int getDivisor(long elemSize) {
    return (int) Math.pow(2, elemSize * 8) / BINS;
  }

  private static int getOffset(long elemSize) {
    return (int) Math.pow(2, elemSize * 8) / 2;
  }
}
