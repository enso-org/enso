package org.enso.table.text;

import org.enso.base.Text_Utils;

public class UnicodeNormalizedFold implements TextFoldingStrategy {
  @Override
  public String fold(String value) {
    return Text_Utils.normalize(value);
  }

  public static final UnicodeNormalizedFold INSTANCE = new UnicodeNormalizedFold();
}
