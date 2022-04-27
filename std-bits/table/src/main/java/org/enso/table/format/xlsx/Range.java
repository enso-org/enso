package org.enso.table.format.xlsx;

import java.util.Optional;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Range {
  private static final Pattern FULL_ADDRESS = Pattern.compile("^('[^']+'|[^'!]+)!(.+)$");

  private static String[] parseFullAddress(String fullAddress) {
    if (fullAddress == null) {
      throw new IllegalArgumentException("fullAddress cannot be NULL.");
    }

    Matcher matcher = FULL_ADDRESS.matcher(fullAddress);
    if (!matcher.find()) {
      throw new IllegalArgumentException("'" + fullAddress + "' is not valid format.");
    }

    return new String[] {matcher.group(1), matcher.group(2)};
  }

  private static final String ADDRESS_A1 = "\\$?[A-Z]{1,3}\\$?\\d+";
  private static final String ADDRESS_COL = "\\$?[A-Z]{1,3}";
  private static final String ADDRESS_ROW = "\\$?\\d+";
  private static final String ADDRESS_RC = "R(?:\\[\\d+]|\\d+)C(?:\\[\\d+]|\\d+)";

  private static final Pattern RANGE_A1 =
      Pattern.compile("(" + ADDRESS_A1 + ")(?::(" + ADDRESS_A1 + "))?");
  private static final Pattern RANGE_COL =
      Pattern.compile("(" + ADDRESS_COL + ")(?::(" + ADDRESS_COL + "))?");
  private static final Pattern RANGE_ROW =
      Pattern.compile("(" + ADDRESS_ROW + ")(?::(" + ADDRESS_ROW + "))?");
  private static final Pattern RANGE_RC =
      Pattern.compile("(" + ADDRESS_RC + ")(?::(" + ADDRESS_RC + "))?");

  private static int[] parseRange(String range) {
    for (Pattern pattern : new Pattern[] {RANGE_A1, RANGE_COL, RANGE_ROW, RANGE_RC}) {
      Optional<int[]> parsed =
          parseRange(range, pattern, pattern == RANGE_RC ? Range::parseRC : Range::parseA1);

      if (parsed.isPresent()) {
        return parsed.get();
      }
    }

    throw new IllegalArgumentException("Unsupported range address: " + range);
  }

  private static Optional<int[]> parseRange(
      String range, Pattern pattern, Function<String, int[]> parser) {
    Matcher matcher = pattern.matcher(range.toUpperCase());
    if (!matcher.find()) {
      return Optional.empty();
    }

    int[] tl = parser.apply(matcher.group(0));
    if (matcher.group(2) == null) {
      return Optional.of(new int[] {tl[0], tl[1], tl[0], tl[1]});
    }

    int[] br = parser.apply(matcher.group(1));
    return Optional.of(
        new int[] {
          Math.min(tl[0], br[0]),
          Math.min(tl[1], br[1]),
          Math.max(tl[0], br[0]),
          Math.max(tl[1], br[1])
        });
  }

  private static boolean isLetter(char c) {
    return c >= 'A' && c <= 'Z';
  }

  private static boolean isDigit(char c) {
    return c >= '0' && c <= '9';
  }

  private static int[] parseA1(CharSequence address) {
    int col = 0;

    int index = 0;
    while (index < address.length() && isLetter(address.charAt(index))) {
      col = 26 * col + (address.charAt(index) - 'A' + 1);
      index++;
    }

    int row = index < address.length() ? Integer.parseInt(address, index, address.length(), 10) : 0;
    return new int[] {row, col};
  }

  private static int[] parseRC(CharSequence address) {
    int index = 0;

    int row = 0;
    if (index < address.length() && address.charAt(index) == 'R') {
      // Parse Row
      int endIndex = index + 1;
      while (endIndex < address.length() && isDigit(address.charAt(endIndex))) {
        endIndex++;
      }

      if (endIndex == index + 1) {
        throw new IllegalArgumentException("R1C1 style addresses must be absolute.");
      }

      row = Integer.parseInt(address, index + 1, endIndex, 10);
    }

    int col = 0;
    if (index < address.length() && address.charAt(index) == 'C') {
      // Parse Row
      int endIndex = index + 1;
      while (endIndex < address.length() && isDigit(address.charAt(endIndex))) {
        endIndex++;
      }

      if (endIndex == index + 1) {
        throw new IllegalArgumentException("R1C1 style addresses must be absolute.");
      }

      col = Integer.parseInt(address, index + 1, endIndex, 10);
    }

    return new int[] {row, col};
  }

  private final String sheetName;
  private final int leftColumn;
  private final int rightColumn;
  private final int topRow;
  private final int bottomRow;

  public Range(String fullAddress) {
    String[] sheetAndRange = parseFullAddress(fullAddress);
    this.sheetName = sheetAndRange[0];

    int[] range = parseRange(sheetAndRange[1]);
    this.leftColumn = range[1];
    this.rightColumn = range[3];
    this.topRow = range[0];
    this.bottomRow = range[2];
  }

  public String getSheetName() {
    return sheetName;
  }

  public boolean isWholeRow() {
    return leftColumn == 0;
  }

  public int getLeftColumn() {
    return leftColumn;
  }

  public int getRightColumn() {
    return rightColumn;
  }

  public boolean isWholeColumn() {
    return topRow == 0;
  }

  public int getTopRow() {
    return topRow;
  }

  public int getBottomRow() {
    return bottomRow;
  }
}
