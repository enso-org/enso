package org.enso.table.read;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.apache.poi.openxml4j.exceptions.InvalidFormatException;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellReference;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.storage.ObjectStorage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.EmptySheetException;
import org.enso.table.error.InvalidLocationException;
import org.enso.table.excel.ExcelConnectionPool;
import org.enso.table.excel.ExcelFileFormat;
import org.enso.table.excel.ExcelHeaders;
import org.enso.table.excel.ExcelRange;
import org.enso.table.excel.ExcelRow;
import org.enso.table.excel.ExcelSheet;
import org.enso.table.excel.ReadOnlyExcelConnection;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

/** A table reader for MS Excel files. */
public class ExcelReader {
  /**
   * Reads a list of sheet names for the specified XLSX/XLS file into an array.
   *
   * @param file the {@link File} to load
   * @param format specifies the file format
   * @return a String[] containing the sheet names.
   * @throws IOException when the action fails
   */
  public static String[] readSheetNames(File file, ExcelFileFormat format)
      throws IOException, InvalidFormatException {
    return withWorkbook(file, format, ExcelReader::readSheetNames);
  }

  /**
   * Reads a list of sheet names from a workbook into an array.
   *
   * @param workbook a {@link Workbook} to read the sheet names from.
   * @return a String[] containing the sheet names.
   */
  public static String[] readSheetNames(Workbook workbook) {
    int sheetCount = workbook.getNumberOfSheets();
    var output = new String[sheetCount];
    Context context = Context.getCurrent();
    for (int i = 0; i < sheetCount; i++) {
      output[i] = workbook.getSheetName(i);
      context.safepoint();
    }
    return output;
  }

  /**
   * Reads a list of range names for the specified XLSX/XLS file into an array.
   *
   * @param file the {@link File} to load
   * @param format specifies the file format
   * @return a String[] containing the range names.
   * @throws IOException when the action fails
   */
  public static String[] readRangeNames(File file, ExcelFileFormat format)
      throws IOException, InvalidFormatException {
    return withWorkbook(file, format, ExcelReader::readRangeNames);
  }

  /**
   * Reads a list of range names for the specified XLSX/XLS file into an array.
   *
   * @param workbook a {@link Workbook} to read the sheet names from.
   * @return a String[] containing the range names.
   */
  public static String[] readRangeNames(Workbook workbook) {
    var names = workbook.getAllNames();
    return names.stream().map(Name::getNameName).toArray(String[]::new);
  }

  /**
   * Reads a sheet by name for the specified XLSX/XLS file into a table.
   *
   * @param file the {@link File} to load
   * @param sheetName the name of the sheet to read.
   * @param skip_rows skip rows from the top the sheet.
   * @param row_limit maximum number of rows to read.
   * @param format specifies the file format
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   * @throws InvalidLocationException when the sheet name is not found.
   */
  public static Table readSheetByName(
      File file,
      String sheetName,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ExcelFileFormat format,
      ProblemAggregator problemAggregator)
      throws IOException, InvalidLocationException {
    return withWorkbook(
        file,
        format,
        workbook -> {
          int sheetIndex = workbook.getSheetIndex(sheetName);
          if (sheetIndex == -1) {
            throw new InvalidLocationException("Unknown sheet '" + sheetName + "'.");
          }

          return readTable(
              workbook,
              sheetIndex,
              null,
              headers,
              skip_rows,
              row_limit == null ? Integer.MAX_VALUE : row_limit,
              problemAggregator);
        });
  }

  /**
   * Reads a sheet by index for the specified XLSX/XLS file into a table.
   *
   * @param file the {@link File} to load
   * @param index the 1-based index to the sheet.
   * @param skip_rows skip rows from the top the sheet.
   * @param row_limit maximum number of rows to read.
   * @param format specifies the file format
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   * @throws InvalidLocationException when the sheet index is not valid.
   */
  public static Table readSheetByIndex(
      File file,
      int index,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ExcelFileFormat format,
      ProblemAggregator problemAggregator)
      throws IOException, InvalidLocationException {
    return withWorkbook(
        file,
        format,
        workbook -> {
          int sheetCount = workbook.getNumberOfSheets();
          if (index < 1 || index > sheetCount) {
            throw new InvalidLocationException(
                "Sheet index is not in valid range (1 to " + sheetCount + " inclusive).");
          }

          return readTable(
              workbook,
              index - 1,
              null,
              headers,
              skip_rows,
              row_limit == null ? Integer.MAX_VALUE : row_limit,
              problemAggregator);
        });
  }

  /**
   * Reads a range by sheet name, named range or address for the specified XLSX/XLS file into a
   * table.
   *
   * @param file the {@link File} to load
   * @param rangeNameOrAddress sheet name, range name or address to read.
   * @param headers specifies whether the first row should be used as headers.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @param format specifies the file format
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   * @throws InvalidLocationException when the range name or address is not found.
   */
  public static Table readRangeByName(
      File file,
      String rangeNameOrAddress,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ExcelFileFormat format,
      ProblemAggregator problemAggregator)
      throws IOException, InvalidLocationException {
    return withWorkbook(
        file,
        format,
        workbook ->
            readRangeByName(
                workbook, rangeNameOrAddress, headers, skip_rows, row_limit, problemAggregator));
  }

  /**
   * Reads a range by sheet name, named range or address for the workbook into a table.
   *
   * @param workbook a {@link Workbook} to read from.
   * @param rangeNameOrAddress sheet name, range name or address to read.
   * @param headers specifies whether the first row should be used as headers.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @return a {@link Table} containing the specified data.
   * @throws InvalidLocationException when the range name or address is not found.
   */
  public static Table readRangeByName(
      Workbook workbook,
      String rangeNameOrAddress,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ProblemAggregator problemAggregator)
      throws InvalidLocationException {
    int sheetIndex = workbook.getSheetIndex(rangeNameOrAddress);
    if (sheetIndex != -1) {
      return readTable(
          workbook,
          sheetIndex,
          null,
          headers,
          skip_rows,
          row_limit == null ? Integer.MAX_VALUE : row_limit,
          problemAggregator);
    }

    Name name = workbook.getName(rangeNameOrAddress);

    ExcelRange excelRange;
    try {
      excelRange = new ExcelRange(name == null ? rangeNameOrAddress : name.getRefersToFormula());
    } catch (IllegalArgumentException e) {
      throw new InvalidLocationException(
          "Invalid range name or address '" + rangeNameOrAddress + "'.");
    }

    return readRange(workbook, excelRange, headers, skip_rows, row_limit, problemAggregator);
  }

  /**
   * Reads a range for the specified XLSX/XLS file into a table.
   *
   * @param file the {@link File} to load
   * @param excelRange the range to read.
   * @param skip_rows skip rows from the top of the range.
   * @param row_limit maximum number of rows to read.
   * @param format specifies the file format
   * @return a {@link Table} containing the specified data.
   * @throws IOException when the input stream cannot be read.
   */
  public static Table readRange(
      File file,
      ExcelRange excelRange,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ExcelFileFormat format,
      ProblemAggregator problemAggregator)
      throws IOException, InvalidLocationException {
    return withWorkbook(
        file,
        format,
        workbook ->
            readRange(workbook, excelRange, headers, skip_rows, row_limit, problemAggregator));
  }

  private static <T> T withWorkbook(File file, ExcelFileFormat format, Function<Workbook, T> action)
      throws IOException {
    try (ReadOnlyExcelConnection connection =
        ExcelConnectionPool.INSTANCE.openReadOnlyConnection(file, format)) {
      return connection.withWorkbook(action);
    }
  }

  private static Table readRange(
      Workbook workbook,
      ExcelRange excelRange,
      ExcelHeaders.HeaderBehavior headers,
      int skip_rows,
      Integer row_limit,
      ProblemAggregator problemAggregator)
      throws InvalidLocationException {
    int sheetIndex = workbook.getSheetIndex(excelRange.getSheetName());
    if (sheetIndex == -1) {
      throw new InvalidLocationException("Unknown sheet '" + excelRange.getSheetName() + "'.");
    }

    return readTable(
        workbook,
        sheetIndex,
        excelRange,
        headers,
        skip_rows,
        row_limit == null ? Integer.MAX_VALUE : row_limit,
        problemAggregator);
  }

  private static Table readTable(
      Workbook workbook,
      int sheetIndex,
      ExcelRange excelRange,
      ExcelHeaders.HeaderBehavior headers,
      int skipRows,
      int rowCount,
      ProblemAggregator problemAggregator) {
    ExcelSheet sheet = new ExcelSheet(workbook, sheetIndex);

    // Expand Single Cell
    if (excelRange != null && excelRange.isSingleCell()) {
      ExcelRow currentRow = sheet.get(excelRange.getTopRow());
      if (currentRow == null || currentRow.isEmpty(excelRange.getLeftColumn())) {
        return new Table(
            new Column[] {
              new Column(
                  CellReference.convertNumToColString(excelRange.getLeftColumn() - 1),
                  new ObjectStorage(new Object[0], 0))
            });
      }

      excelRange = ExcelRange.expandSingleCell(excelRange, sheet);
    }

    // Row Range
    boolean wholeColumn = excelRange == null || excelRange.isWholeColumn();
    int startRow = (wholeColumn ? 1 : excelRange.getTopRow()) + skipRows;
    int endRow = wholeColumn ? sheet.getLastRow() : excelRange.getBottomRow();

    // Column Range
    boolean wholeRow = excelRange == null || excelRange.isWholeRow();
    int startCol = wholeRow ? 1 : excelRange.getLeftColumn();
    int endCol = wholeRow ? -1 : excelRange.getRightColumn();

    // Headers
    ExcelHeaders excelHeaders =
        new ExcelHeaders(
            headers,
            sheet.get(startRow),
            startRow < endRow ? sheet.get(startRow + 1) : null,
            startCol,
            endCol,
            problemAggregator);
    startRow += excelHeaders.getRowsUsed();

    // Set up Storage
    int size = Math.min(rowCount, endRow - startRow + 1);
    List<Builder> builders =
        wholeRow
            ? new ArrayList<>()
            : IntStream.range(startCol, endCol + 1)
                .mapToObj(i -> new InferredBuilder(size, problemAggregator))
                .collect(Collectors.toList());

    // Read Cell Data
    int row = startRow;
    while (row <= endRow && (row - startRow) < rowCount) {
      ExcelRow currentRow = sheet.get(row);
      if (currentRow == null) {
        builders.forEach(b -> b.append(null));
      } else {
        int currentEndCol =
            endCol == -1
                ? Math.max(currentRow.getLastColumn(), startCol + builders.size() - 1)
                : endCol;
        expandBuilders(builders, size, currentEndCol - startCol, row - startRow, problemAggregator);

        for (int col = startCol; col <= currentEndCol; col++) {
          Object value = currentRow.getCellValue(col);
          builders.get(col - startCol).append(value);
        }
      }

      row++;
    }

    // Special case for stopping before firstRow
    if (wholeRow && (rowCount == 0 || row < sheet.getFirstRow())) {
      ExcelRow currentRow = sheet.get(sheet.getFirstRow());
      int currentEndCol = currentRow.getLastColumn();
      expandBuilders(builders, size, currentEndCol - startCol + 1, size, problemAggregator);
    }

    // Create Table
    Column[] columns =
        IntStream.range(0, builders.size())
            .mapToObj(idx -> new Column(excelHeaders.get(idx + startCol), builders.get(idx).seal()))
            .toArray(Column[]::new);

    if (columns.length == 0) {
      throw new EmptySheetException();
    }

    return new Table(columns);
  }

  private static void expandBuilders(
      List<Builder> builders,
      int size,
      int columnCount,
      int rows,
      ProblemAggregator problemAggregator) {
    for (int i = builders.size(); i <= columnCount; i++) {
      Builder builder = new InferredBuilder(size, problemAggregator);
      builder.appendNulls(rows);
      builders.add(builder);
    }
  }
}
