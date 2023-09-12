package org.enso.table_test_helpers;

import java.math.BigInteger;

public class PolyglotHelpers {
  /* Creates a big integer that is larger than long can fit. */
  public static BigInteger createBigBigIntegerComingFromJava() {
    return BigInteger.TWO.pow(70).subtract(BigInteger.ONE);
  }

  /* Creates a big integer that could fit in a long. */
  public static BigInteger createSmallBigIntegerComingFromJava() {
    return BigInteger.TEN;
  }
}
