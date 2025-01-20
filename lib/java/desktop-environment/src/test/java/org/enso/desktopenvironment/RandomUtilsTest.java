package org.enso.desktopenvironment;

import org.junit.Assert;
import org.junit.Test;

public class RandomUtilsTest {

  @Test
  public void alphanumericString() {
    var size = Short.MAX_VALUE;
    var str = RandomUtils.alphanumericString(size);
    Assert.assertEquals(size, str.length());
    Assert.assertTrue(isAsciiAlphanumeric(str));
  }

  @Test
  public void alphanumericStringEmpty() {
    var size = 0;
    var str = RandomUtils.alphanumericString(size);
    Assert.assertEquals(size, str.length());
  }

  @Test
  public void alphanumericStringNegativeSize() {
    Assert.assertThrows(IllegalArgumentException.class, () -> RandomUtils.alphanumericString(-1));
  }

  private static boolean isAsciiAlphanumeric(String str) {
    boolean result = true;
    for (int i = 0; i < str.length(); i++) {
      var c = str.charAt(i);
      result = result && (isAsciiLetter(c) || isAsciiDigit(c));
    }

    return result;
  }

  private static boolean isAsciiLetter(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
  }

  private static boolean isAsciiDigit(char c) {
    return c >= '0' && c <= '9';
  }
}
