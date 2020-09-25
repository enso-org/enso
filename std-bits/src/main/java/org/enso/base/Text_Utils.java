package org.enso.base;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.regex.Pattern;

public class Text_Utils {
  public static String substring(String string, int from, int to) {
    return string.substring(from, to);
  }

  public static byte[] get_bytes(String str) {
    return str.getBytes(StandardCharsets.UTF_8);
  }

  public static int[] get_codepoints(String str) {
    return str.codePoints().toArray();
  }

  public static String[] split_at(String str, String sep) {
    return str.split(Pattern.quote(sep));
  }
}
