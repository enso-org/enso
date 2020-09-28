package org.enso.base;

import com.ibm.icu.text.Normalizer2;

import java.nio.charset.StandardCharsets;
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

  public static boolean equals(String str1, String str2) {
    return Normalizer2.getNFDInstance()
        .normalize(str1)
        .equals(Normalizer2.getNFDInstance().normalize(str2));
  }

  public static String from_codepoints(int[] codepoints) {
    return new String(codepoints, 0, codepoints.length);
  }

  public static String from_utf_8(byte[] bytes) {
    return new String(bytes, StandardCharsets.UTF_8);
  }
}
