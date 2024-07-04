package org.enso.desktopenvironment;

import java.util.Random;

final class RandomUtils {

  private static final char[] ALPHANUMERIC =
      new char[] {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
        'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'J', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
      };

  private RandomUtils() {}

  /** Delay the {@link Random} instance initialization when building the native image. */
  private static final class LazyRandom {

    private static Random instance = null;

    private LazyRandom() {}

    public static Random getInstance() {
      if (instance == null) {
        instance = new Random();
      }
      return instance;
    }
  }

  /**
   * Get random alphanumeric ASCII string.
   *
   * @param size the size of the result string.
   * @return the random string.
   */
  public static synchronized String alphanumericString(int size) {
    if (size < 0) {
      throw new IllegalArgumentException("String size should be positive.");
    }

    var random = LazyRandom.getInstance();
    var builder = new StringBuilder(size);
    while (builder.length() < size) {
      builder.append(ALPHANUMERIC[random.nextInt(ALPHANUMERIC.length)]);
    }

    return builder.toString();
  }
}
