/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * The code is based on the reference Java implementation of Arrow specification.
 */

package org.enso.interpreter.arrow.runtime;

public class RoundingUtil {

  /** The mask for rounding an integer to a multiple of 8. (i.e. clear the lowest 3 bits) */
  static int ROUND_8_MASK_INT = 0xFFFFFFF8;

  /** The mask for rounding a long integer to a multiple of 8. (i.e. clear the lowest 3 bits) */
  static long ROUND_8_MASK_LONG = 0xFFFFFFFFFFFFFFF8L;

  /** The number of bits to shift for dividing by 8. */
  static int DIVIDE_BY_8_SHIFT_BITS = 3;

  private RoundingUtil() {}

  /* round up bytes for the validity buffer for the given valueCount */
  private static long roundUp8ForValidityBuffer(long valueCount) {
    return ((valueCount + 63) >> 6) << 3;
  }

  /**
   * Round up the number to the nearest multiple of 8.
   *
   * @param input the number to round.
   * @return the rounded number.
   */
  private static int roundUpTo8Multiple(int input) {
    return (input + 7) & ROUND_8_MASK_INT;
  }

  /**
   * Round up the number to the nearest multiple of 8.
   *
   * @param input the number to round.
   * @return the rounded number
   */
  private static long roundUpTo8Multiple(long input) {
    return (input + 7L) & ROUND_8_MASK_LONG;
  }

  public static PaddedSize forValueCount(int valueCount, SizeInBytes unit) {
    return new PaddedSize(valueCount, unit);
  }

  // Encapsulates the calculation of padding as requred by Arrow specification
  static class PaddedSize {
    private SizeInBytes unit;

    private int valueCount;

    /**
     * Returns the number of bytes that should be allocated to the null bitmap with padding
     * included.
     *
     * @return number of bytes
     */
    public int getValidityBitmapSizeInBytes() {
      return (int) validityBitmapSize;
    }

    /**
     * Returns the number of bytes that should be allocated to the data buffer with padding
     * included.
     *
     * @return number of bytes
     */
    public int getDataBufferSizeInBytes() {
      return (int) dataBufferSize;
    }

    public int getTotalSizeInBytes() {
      return (int) (dataBufferSize + validityBitmapSize);
    }

    private long validityBitmapSize;
    private long dataBufferSize;

    private PaddedSize(int valueCount, SizeInBytes unit) {
      this.valueCount = valueCount;
      this.unit = unit;
      computeBufferSize(valueCount, unit);
    }

    private long defaultRoundedSize(long val) {
      if (val == 0 || val == 1) {
        return val + 1;
      }
      long highestBit = Long.highestOneBit(val);
      if (highestBit == val) {
        return val;
      } else {
        return highestBit << 1;
      }
    }

    long computeCombinedBufferSize(int valueCount, int typeWidth) {

      // compute size of null-bitmap buffer.
      long bufferSize = roundUp8ForValidityBuffer(valueCount);

      // add the size of the data buffer.
      if (typeWidth == 0) {
        // for boolean type, data-buffer and null-bitmap-buffer are of same size.
        bufferSize *= 2;
      } else {
        bufferSize += roundUpTo8Multiple((long) valueCount * typeWidth);
      }
      return defaultRoundedSize(bufferSize);
    }

    private void computeBufferSize(int valueCount, SizeInBytes unit) {
      var typeWidth = unit.sizeInBytes();
      long bufferSize = computeCombinedBufferSize(valueCount, typeWidth);
      assert bufferSize <= Long.MAX_VALUE;

      long validityBufferSize;
      long dataBufferSize;
      if (typeWidth == 0) {
        validityBufferSize = dataBufferSize = bufferSize / 2;
      } else {
        // Due to the rounding policy, the bufferSize could be greater than the
        // requested size. Utilize the allocated buffer fully.;
        long actualCount = (long) ((bufferSize * 8.0) / (8 * typeWidth + 1));
        do {
          validityBufferSize = roundUp8ForValidityBuffer(actualCount);
          dataBufferSize = roundUpTo8Multiple(actualCount * typeWidth);
          if (validityBufferSize + dataBufferSize <= bufferSize) {
            break;
          }
          --actualCount;
        } while (true);
      }
      this.validityBitmapSize = validityBufferSize;
      this.dataBufferSize = dataBufferSize;
    }
  }
}
