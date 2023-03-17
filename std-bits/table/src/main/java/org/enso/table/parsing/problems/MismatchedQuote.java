package org.enso.table.parsing.problems;

/**
 * Indicates that a Delimited file is corrupted because it contains a quote that was opened and
 * never closed.
 */
public class MismatchedQuote extends RuntimeException {
  private final String cellText;

  public MismatchedQuote(String cellText) {
    this.cellText = cellText;
  }

  public String getCellText() {
    return cellText;
  }
}
