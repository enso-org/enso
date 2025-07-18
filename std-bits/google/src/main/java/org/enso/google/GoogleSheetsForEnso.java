package org.enso.google;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import java.util.stream.IntStream;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.EmptySheetException;
import org.enso.table.problems.ProblemAggregator;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.SheetsScopes;
import com.google.api.services.sheets.v4.model.RowData;
import com.google.auth.http.HttpCredentialsAdapter;

public class GoogleSheetsForEnso {

  private final Sheets service;

  private GoogleSheetsForEnso(Sheets service) {
    this.service = service;
  }

  public static GoogleSheetsForEnso create(WrappedGoogleCredentials credentials)
      throws GeneralSecurityException, IOException {
    var credentialsAdapter =
        new HttpCredentialsAdapter(
            CredentialsHelper.materialize(credentials).createScoped(SheetsScopes.SPREADSHEETS));
    Sheets.Builder builder =
        new Sheets.Builder(
                GoogleNetHttpTransport.newTrustedTransport(),
                GsonFactory.getDefaultInstance(),
                credentialsAdapter)
            .setApplicationName("Enso");
    return new GoogleSheetsForEnso(builder.build());
  }

  public Table getSheetRange(
      String sheetId,
      String range,
      GoogleSheetsHeaders.HeaderBehavior headerBehavior,
      Integer row_limit,
      int skip_rows,
      ProblemAggregator problemAggregator)
      throws IOException {
    var rawData =
        service
            .spreadsheets()
            .values()
            .get(sheetId, range)
            .setMajorDimension("COLUMNS")
            .setValueRenderOption("UNFORMATTED_VALUE")
            .execute()
            .getValues();

    if (rawData == null) {
      throw new EmptySheetException();
    }

    var rowData = service
      .spreadsheets()
      .get(sheetId)
      .setRanges(List.of(range))
      .setIncludeGridData(true)
      .execute()
      .getSheets().get(0)
      .getData().get(0)
      .getRowData();

    final int firstRowIndex = Math.max(0, skip_rows);
    var firstRow = getDataRow(rowData, firstRowIndex);
    var secondRow = getDataRow(rowData, firstRowIndex + 1);
    GoogleSheetsHeaders headerBuilder =
        new GoogleSheetsHeaders(headerBehavior, firstRow, secondRow, problemAggregator);

    var numberOfColumns = firstRow.getValues().size();
    Column[] columns = new Column[numberOfColumns];
    var resolved_row_limit = row_limit == null ? Long.MAX_VALUE : (row_limit < 0 ? 0 : row_limit);
    for (int colIdx = 0; colIdx < numberOfColumns; colIdx++) {
      var column = rawData.get(colIdx);
      var builder = Builder.getInferredBuilder(column.size(), problemAggregator);
      IntStream.range(0, column.size())
          .skip(firstRowIndex)
          .skip(headerBuilder.getRowsUsed())
          .limit(resolved_row_limit)
          .mapToObj(rowIdx -> fixTypes(column.get(rowIdx)))
          .forEach(builder::append);
      columns[colIdx] = new Column(headerBuilder.get(colIdx), builder.seal());
    }
    return new Table(columns);
  }

  private static Object fixTypes(Object value) {
    if (value instanceof String str && str.isEmpty()) {
      return null;
    }
    if (value instanceof java.math.BigDecimal bd && bd.scale() <= 0) {
      int intValue = bd.intValue();
      return intValue;
    }
    return value;
  }

  private com.google.api.services.sheets.v4.model.Spreadsheet getSpreadsheet(String workbookId)
      throws IOException {
    return service.spreadsheets().get(workbookId).setIncludeGridData(false).execute();
  }

  public int getNumberOfSheets(String workbookId) throws IOException {
    return getSpreadsheet(workbookId).getSheets().size();
  }

  public List<String> getSheetNames(String workbookId) throws IOException {
    return getSpreadsheet(workbookId).getSheets().stream()
        .map(sheet -> sheet.getProperties().getTitle())
        .toList();
  }

  public int getNumberOfNames(String workbookId) throws IOException {
    var namedRanges = getSpreadsheet(workbookId).getNamedRanges();
    return namedRanges == null ? 0 : namedRanges.size();
  }

  public List<String> getRangeNames(String workbookId) throws IOException {
    var namedRanges = getSpreadsheet(workbookId).getNamedRanges();
    return namedRanges == null
        ? List.of()
        : namedRanges.stream().map(range -> range.getName()).toList();
  }

  private static RowData getDataRow(List<RowData> rowData, int rowIndex) {
    if (rowData.size()>rowIndex) {
      return rowData.get(rowIndex);
    }
    return null;
  }
}
