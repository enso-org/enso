package org.enso.table.text;

import org.enso.base.Text_Utils;

import java.util.Locale;

public class CaseInsensitiveFold implements TextFoldingStrategy {

  private final Locale locale;

  public CaseInsensitiveFold(Locale locale) {
    this.locale = locale;
  }

  @Override
  public String fold(String value) {
    return Text_Utils.case_insensitive_key(value, locale);
  }
}
