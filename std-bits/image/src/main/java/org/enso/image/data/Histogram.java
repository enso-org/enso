package org.enso.image.data;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfFloat;
import org.opencv.core.MatOfInt;
import org.opencv.imgproc.Imgproc;

import java.util.ArrayList;
import java.util.List;

/** A histogram calculated for a single channel of an image. */
public class Histogram {

  private static final int BINS = 256;
  private static final int MAX_VALUE = 256;

  private final int[] data;
  private final int channel;

  /**
   * Create the histogram.
   *
   * @param data the array of bins.
   * @param channel the analyzed channel.
   */
  public Histogram(int[] data, int channel) {
    this.data = data;
    this.channel = channel;
  }

  /**
   * Calculate a histogram of the image channel.
   *
   * @param image the input image.
   * @param channel the channel to analyze
   * @return the calculated histogram.
   */
  public static Histogram calculate(Mat image, int channel) {
    Mat histogram = new Mat();
    MatOfFloat valuesRange = new MatOfFloat(0, MAX_VALUE);
    List<Mat> images = new ArrayList<>();
    Core.split(image, images);

    Imgproc.calcHist(
        images, new MatOfInt(channel), new Mat(), histogram, new MatOfInt(BINS), valuesRange);
    Core.normalize(histogram, histogram, 0, MAX_VALUE, Core.NORM_MINMAX);

    float[] histogramData = new float[(int) histogram.total() * histogram.channels()];
    histogram.get(0, 0, histogramData);
    int[] binData = new int[histogramData.length];
    for (int i = 0; i < binData.length; i++) {
      binData[i] = Math.round(histogramData[i]);
    }

    return new Histogram(binData, channel);
  }

  /** @return the channel number. */
  public int get_channel() {
    return channel;
  }

  /** @return the histogram data. */
  public int[] get_data() {
    return data;
  }
}
