package org.enso.table.excel;

import java.util.Optional;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellReference;

public class ExcelRange {
  private static final Pattern FULL_ADDRESS = Pattern.compile("^('.+'|[^'!]+)!(.+)$");
  private static final int WHOLE_ROW_OR_COLUMN = -1;

  private static String[] parseFullAddress(String fullAddress) throws IllegalArgumentException {
    if (fullAddress == null) {
      throw new IllegalArgumentException("fullAddress cannot be NULL.");
    }

    Matcher matcher = FULL_ADDRESS.matcher(fullAddress);
    if (!matcher.find()) {
      throw new IllegalArgumentException("'" + fullAddress + "' is not valid format.");
    }

    return new String[] {matcher.group(1), matcher.group(2)};
  }

  private static String unescapeSheetName(String sheetName) {
    return sheetName.replaceAll("^'(.*)'$", "$1").replaceAll("''", "'");
  }

  private static final String ADDRESS_A1 = "\\$?[A-Z]{1,3}\\$?\\d+";
  private static final String ADDRESS_COL = "\\$?[A-Z]{1,3}";
  private static final String ADDRESS_ROW = "\\$?\\d+";
  private static final String ADDRESS_RC = "R(?:\\[\\d+]|\\d+)C(?:\\[\\d+]|\\d+)";

  private static final Pattern RANGE_A1 =
      Pattern.compile("^(" + ADDRESS_A1 + ")(?::(" + ADDRESS_A1 + "))?$");
  private static final Pattern RANGE_COL =
      Pattern.compile("^(" + ADDRESS_COL + ")(?::(" + ADDRESS_COL + "))?$");
  private static final Pattern RANGE_ROW =
      Pattern.compile("^(" + ADDRESS_ROW + ")(?::(" + ADDRESS_ROW + "))?$");
  private static final Pattern RANGE_RC =
      Pattern.compile("^(" + ADDRESS_RC + ")(?::(" + ADDRESS_RC + "))?$");

  private static int[] parseRange(String range) throws IllegalArgumentException {
    for (Pattern pattern : new Pattern[] {RANGE_A1, RANGE_COL, RANGE_ROW, RANGE_RC}) {
      Optional<int[]> parsed =
          parseRange(
              range,
              pattern,
              pattern == RANGE_RC
                  ? ExcelRange::parseR1C1StyleAddress
                  : ExcelRange::parseA1StyleAddress);

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

    int[] tl = parser.apply(matcher.group(1));
    if (matcher.group(2) == null) {
      return Optional.of(new int[] {tl[0], tl[1]});
    }

    int[] br = parser.apply(matcher.group(2));
    return Optional.of(
        new int[] {
          Math.min(tl[0], br[0]),
          Math.min(tl[1], br[1]),
          Math.max(tl[0], br[0]),
          Math.max(tl[1], br[1])
        });
  }

  private static boolean isLetter(char c) {
    // Cannot use the isLetter function as must be explicitly A-Z.
    return c >= 'A' && c <= 'Z';
  }

  private static boolean isDigit(char c) {
    // Cannot use the isDigit function as must be explicitly 0-9.
    return c >= '0' && c <= '9';
  }

  private static int skipDollar(CharSequence address, int index) {
    if (index < address.length() - 1 && address.charAt(index) == '$') {
      index++;
    }
    return index;
  }

  private static int[] parseA1StyleAddress(CharSequence address) {
    ParsedInteger col = parseColumn(address, skipDollar(address, 0));
    ParsedInteger row = parseInteger(address, skipDollar(address, col.index));
    return new int[] {row.value == 0 ? WHOLE_ROW_OR_COLUMN : row.value, col.value};
  }

  private static int[] parseR1C1StyleAddress(CharSequence address) throws IllegalArgumentException {
    int index = 0;

    int row = 0;
    if (index < address.length() && address.charAt(index) == 'R') {
      ParsedInteger parsed = parseInteger(address, index + 1);
      if (parsed.value == 0) {
        throw new IllegalArgumentException(address + " not an absolute R1C1 style addresses.");
      }

      row = parsed.value;
      index = parsed.index;
    }

    int col = 0;
    if (index < address.length() && address.charAt(index) == 'C') {
      ParsedInteger parsed = parseInteger(address, index + 1);
      if (parsed.value == 0) {
        throw new IllegalArgumentException(address + " not an absolute R1C1 style addresses.");
      }

      col = parsed.value;
    }

    return new int[] {row, col};
  }

  /**
   * Convert an Excel Column Name (e.g. DCR) into the index (1-based)
   *
   * @param column name
   * @return Column index (A=1 ...)
   */
  public static int parseA1Column(CharSequence column) throws IllegalArgumentException {
    ParsedInteger parsed = parseColumn(column, skipDollar(column, 0));
    if (parsed.index != column.length() || parsed.value == WHOLE_ROW_OR_COLUMN) {
      throw new IllegalArgumentException(column + " is not a valid Excel Column Name.");
    }

    return parsed.value;
  }

  /**
   * Create an ExcelRange for a given set of columns.
   *
   * @param sheetName name of the containing sheet.
   * @param leftColumn left-hand column index (1-based).
   * @param rightColumn inclusive right-hand column index (1-based).
   * @return ExcelRange for the given columns.
   */
  public static ExcelRange forColumns(String sheetName, int leftColumn, int rightColumn) {
    return new ExcelRange(
        sheetName, leftColumn, WHOLE_ROW_OR_COLUMN, rightColumn, WHOLE_ROW_OR_COLUMN);
  }

  /**
   * Create an ExcelRange for a given set of rows.
   *
   * @param sheetName name of the containing sheet.
   * @param topRow top row index (1-based).
   * @param bottomRow inclusive bottom row index (1-based).
   * @return ExcelRange for the given rows.
   */
  public static ExcelRange forRows(String sheetName, int topRow, int bottomRow) {
    return new ExcelRange(sheetName, WHOLE_ROW_OR_COLUMN, topRow, WHOLE_ROW_OR_COLUMN, bottomRow);
  }

  /**
   * Given a single cell and the containing sheet, expand the range to cover the connected table of
   * cells.
   *
   * @param excelRange Range referring to top left cell.
   * @param sheet ExcelSheet containing the range refers to.
   * @return Expanded range covering the connected table of cells.
   */
  public static ExcelRange expandSingleCell(ExcelRange excelRange, ExcelSheet sheet) {
    ExcelRow currentRow = sheet.get(excelRange.getTopRow());
    if (currentRow == null || currentRow.isEmpty(excelRange.getLeftColumn())) {
      return new ExcelRange(
          excelRange.getSheetName(),
          excelRange.getLeftColumn(),
          excelRange.getTopRow(),
          excelRange.getLeftColumn(),
          excelRange.getTopRow());
    }

    int bottomRow = excelRange.getTopRow();
    int rightColumn = excelRange.getLeftColumn();

    while (currentRow != null && !currentRow.isEmpty(excelRange.getLeftColumn(), rightColumn)) {
      rightColumn = currentRow.findEndRight(rightColumn);
      bottomRow++;
      currentRow = sheet.get(bottomRow);
    }

    return new ExcelRange(
        excelRange.getSheetName(),
        excelRange.getLeftColumn(),
        excelRange.getTopRow(),
        rightColumn,
        bottomRow - 1);
  }

  /**
   * @param index Index to the next character after the parsed value
   * @param value Parsed integer value or 0 if not valid
   */
  private record ParsedInteger(int index, int value) {}

  private static ParsedInteger parseInteger(CharSequence address, int index) {
    int endIndex = index;
    while (endIndex < address.length() && isDigit(address.charAt(endIndex))) {
      endIndex++;
    }
    return new ParsedInteger(
        endIndex, endIndex == index ? 0 : Integer.parseInt(address, index, endIndex, 10));
  }

  private static ParsedInteger parseColumn(CharSequence column, int startIndex) {
    int col = 0;

    int index = startIndex;
    while (index < column.length() && isLetter(column.charAt(index))) {
      col = 26 * col + (column.charAt(index) - 'A' + 1);
      index++;
    }

    return new ParsedInteger(index, col == 0 ? WHOLE_ROW_OR_COLUMN : col);
  }

  private final String sheetName;
  private final int leftColumn;
  private final int rightColumn;
  private final int topRow;
  private final int bottomRow;
  private final boolean singleCell;

  public ExcelRange(String fullAddress) throws IllegalArgumentException {
    String[] sheetAndRange = parseFullAddress(fullAddress);
    this.sheetName = unescapeSheetName(sheetAndRange[0]);

    int[] range = parseRange(sheetAndRange[1]);
    this.leftColumn = range[1];
    this.topRow = range[0];

    if (range.length == 2) {
      this.singleCell = range[0] != WHOLE_ROW_OR_COLUMN && range[1] != WHOLE_ROW_OR_COLUMN;
      this.rightColumn = range[1];
      this.bottomRow = range[0];
    } else {
      this.singleCell = false;
      this.rightColumn = range[3];
      this.bottomRow = range[2];
    }
  }

  public ExcelRange(String sheetName, int column, int row) {
    this.sheetName = sheetName;
    this.singleCell = column != WHOLE_ROW_OR_COLUMN && row != WHOLE_ROW_OR_COLUMN;
    this.leftColumn = column;
    this.topRow = row;
    this.rightColumn = column;
    this.bottomRow = row;
  }

  public ExcelRange(String sheetName, int leftColumn, int topRow, int rightColumn, int bottomRow) {
    this.sheetName = sheetName;
    this.singleCell = false;
    this.leftColumn = Math.min(leftColumn, rightColumn);
    this.topRow = Math.min(bottomRow, topRow);
    this.rightColumn = Math.max(leftColumn, rightColumn);
    this.bottomRow = Math.max(bottomRow, topRow);
  }

  public String getSheetName() {
    return sheetName;
  }

  public String getEscapedSheetName() {
    String sheetNameEscaped = sheetName;
    if (sheetNameEscaped.contains(" ") || sheetNameEscaped.contains("'")) {
      sheetNameEscaped = "'" + sheetNameEscaped.replace("'", "''") + "'";
    }
    return sheetNameEscaped;
  }

  public boolean isWholeRow() {
    return leftColumn == WHOLE_ROW_OR_COLUMN;
  }

  public int getLeftColumn() {
    return leftColumn;
  }

  public int getRightColumn() {
    return rightColumn;
  }

  public int getColumnCount() {
    return isWholeRow() ? Integer.MAX_VALUE : rightColumn - leftColumn + 1;
  }

  public boolean isWholeColumn() {
    return topRow == WHOLE_ROW_OR_COLUMN;
  }

  public int getTopRow() {
    return topRow;
  }

  public int getBottomRow() {
    return bottomRow;
  }

  public int getRowCount() {
    return isWholeColumn() ? Integer.MAX_VALUE : bottomRow - topRow + 1;
  }

  public int getLastNonEmptyRow(ExcelSheet sheet) {
    int lastRow =
        Math.min(sheet.getLastRow(), isWholeColumn() ? sheet.getLastRow() : bottomRow) + 1;

    while (lastRow > topRow
        && sheet.get(lastRow - 1).isEmpty(leftColumn, isWholeRow() ? -1 : rightColumn)) {
      lastRow--;
    }

    return lastRow - 1;
  }

  public boolean isSingleCell() {
    return this.singleCell;
  }

  public String getAddress() {
    String sheetNameEscaped = getEscapedSheetName();

    String range =
        (isWholeRow() ? "" : CellReference.convertNumToColString(getLeftColumn() - 1))
            + (isWholeColumn() ? "" : Integer.toString(getTopRow()));
    if (getLeftColumn() != getRightColumn() || getTopRow() != getBottomRow()) {
      range +=
          ":"
              + (isWholeRow() ? "" : CellReference.convertNumToColString(getRightColumn() - 1))
              + (isWholeColumn() ? "" : Integer.toString(getBottomRow()));
    }

    return sheetNameEscaped + "!" + range;
  }

  public ExcelRange getAbsoluteRange(Workbook workbook) {
    int topRow = isWholeColumn() ? 1 : getTopRow();
    int bottomRow =
        isWholeColumn() ? workbook.getSpreadsheetVersion().getMaxRows() : getBottomRow();
    int leftColumn = isWholeRow() ? 1 : getLeftColumn();
    int rightColumn =
        isWholeRow() ? workbook.getSpreadsheetVersion().getMaxColumns() : getRightColumn();
    return new ExcelRange(getSheetName(), leftColumn, topRow, rightColumn, bottomRow);
  }
}
