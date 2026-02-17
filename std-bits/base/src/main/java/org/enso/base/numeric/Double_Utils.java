package org.enso.base.numeric;

/** Utils for working with doubles. */
public final class Double_Utils {
  /**
   * Compares two double values to see if they are equal within 1 ULP (Unit in the Last Place).
   *
   * @param a the first double value
   * @param b the second double value
   * @return true if the values are equal within 1 ULP, false otherwise
   */
  public static boolean compareULP(double a, double b) {
    long aBits = Double.doubleToLongBits(a);
    long bBits = Double.doubleToLongBits(b);

    // Make aBits and bBits lexicographically ordered as a twos-complement long
    if (aBits < 0) {
      aBits = 0x8000000000000000L - aBits;
    }
    if (bBits < 0) {
      bBits = 0x8000000000000000L - bBits;
    }

    long ulpDiff = Math.abs(aBits - bBits);
    return ulpDiff <= 1;
  }
}
