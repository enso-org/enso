package org.enso.table.format.xlsx;

import java.util.regex.Pattern;

public class Range {
  private static Pattern pattern = new Pattern("");

  private final String sheetName;
  private final int leftColumn;
  private final int rightColumn;
  private final int topRow;
  private final int bottowRow;

  public Range(String rangeAddress) {

  }

  // ^('([^']+)'|([^'!]+))!([A-Z]+\d+|R\d+C\d+)(:([A-Z]+\d+))?$
  // \$?[A-Z]{1,3}\$?\d+
  // (\$?[A-Z]{1,3}\$?\d+)(?::(\$?[A-Z]{1,3}\$?\d+))?
}
