package org.enso.google;

import static org.enso.table.excel.ExcelUtils.fromExcelDateTime;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.googleapis.json.GoogleJsonResponseException;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.SheetsScopes;
import com.google.api.services.sheets.v4.model.CellData;
import com.google.api.services.sheets.v4.model.RowData;
import com.google.api.services.sheets.v4.model.Sheet;
import com.google.api.services.sheets.v4.model.Spreadsheet;
import com.google.auth.http.HttpCredentialsAdapter;
import java.io.IOException;
import java.time.temporal.Temporal;
import java.util.List;
import java.util.Optional;
import org.enso.base.polyglot.EnsoExceptionWrapper;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.table.Column;
import org.enso.table.error.EmptySheetException;
import org.enso.table.problems.ProblemAggregator;
import org.enso.table.util.LeastRecentlyUsedCache;
import org.graalvm.polyglot.Value;

public class GoogleSheetsForEnso {
  private final Sheets service;

  private record CacheKey(String sheetId, String range) {}

  private final LeastRecentlyUsedCache<String, Spreadsheet> spreadsheetCache;
  private final LeastRecentlyUsedCache<CacheKey, Sheet> sheetCache;

  private GoogleSheetsForEnso(Sheets service) {
    this.service = service;
    this.spreadsheetCache = new LeastRecentlyUsedCache<>(10);
    this.sheetCache = new LeastRecentlyUsedCache<>(50);
  }

  private Spreadsheet getSpreadsheetWithCache(String sheetId) throws IOException {
    if (spreadsheetCache.containsKey(sheetId)) {
      return spreadsheetCache.get(sheetId);
    }

    var spreadsheet = service.spreadsheets().get(sheetId).execute();
    spreadsheetCache.put(sheetId, spreadsheet);
    return spreadsheet;
  }

  private Sheet getSheetWithCache(String sheetId, String range) throws IOException {
    var cacheKey = new CacheKey(sheetId, range);
    if (sheetCache.containsKey(cacheKey)) {
      return sheetCache.get(cacheKey);
    }

    var spreadsheet =
        service
            .spreadsheets()
            .get(sheetId)
            .setRanges(List.of(range))
            .setIncludeGridData(true)
            .execute()
            .getSheets()
            .get(0);
    sheetCache.put(cacheKey, spreadsheet);
    return spreadsheet;
  }

  /**
   * Batch fetches the specified ranges for the given sheet ID, utilizing caching to optimize
   * performance.
   */
  public Value batchFetchRanges(String sheetId, List<String> ranges) {
    try {
      var cacheKeys =
          ranges.stream()
              .map(range -> new CacheKey(sheetId, range))
              .filter(cacheKey -> !sheetCache.containsKey(cacheKey))
              .toList();

      if (!cacheKeys.isEmpty()) {
        var request =
            service
                .spreadsheets()
                .get(sheetId)
                .setRanges(cacheKeys.stream().map(CacheKey::range).toList())
                .setIncludeGridData(true);
        var spreadsheet = request.execute().getSheets();

        for (int i = 0; i < cacheKeys.size(); i++) {
          sheetCache.put(cacheKeys.get(i), spreadsheet.get(i));
        }
      }

      return Value.asValue(cacheKeys.size());
    } catch (Exception e) {
      return wrapJavaException(sheetId, e);
    }
  }

  /** Creates a new instance of GoogleSheetsForEnso using the provided credentials. */
  public static Value create(WrappedGoogleCredentials credentials) {
    try {
      var credentialsAdapter =
          new HttpCredentialsAdapter(
              CredentialsHelper.materialize(credentials).createScoped(SheetsScopes.SPREADSHEETS));
      Sheets.Builder builder =
          new Sheets.Builder(
                  GoogleNetHttpTransport.newTrustedTransport(),
                  GsonFactory.getDefaultInstance(),
                  credentialsAdapter)
              .setApplicationName("Enso");
      var output = new GoogleSheetsForEnso(builder.build());
      return Value.asValue(output);
    } catch (Exception e) {
      return wrapJavaException(null, e);
    }
  }

  public Value getSheetRange(
      String sheetId,
      String range,
      GoogleSheetsHeaders.HeaderBehavior headerBehavior,
      Integer row_limit,
      int skip_rows,
      boolean reportAsErrors) {
    try {
      var rowData = getSheetWithCache(sheetId, range).getData().get(0).getRowData();

      if (rowData == null) {
        throw new EmptySheetException();
      }

      var problemAggregator = ProblemAggregator.makeTopLevelAggregator();

      final int firstRowIndex = Math.max(0, skip_rows);
      var firstRow = getDataRow(rowData, firstRowIndex);
      var secondRow = getDataRow(rowData, firstRowIndex + 1);
      GoogleSheetsHeaders headerBuilder =
          new GoogleSheetsHeaders(headerBehavior, firstRow, secondRow, problemAggregator);

      var numberOfColumns = firstRow.getValues().size();
      Builder[] builders = new Builder[numberOfColumns];
      for (int i = 0; i < numberOfColumns; i++) {
        builders[i] = Builder.getInferredBuilder(rowData.size(), problemAggregator);
      }
      var resolved_row_limit = row_limit == null ? Long.MAX_VALUE : (row_limit < 0 ? 0 : row_limit);

      rowData.stream()
          .skip(firstRowIndex)
          .skip(headerBuilder.getRowsUsed())
          .limit(resolved_row_limit)
          .forEach(
              row -> {
                for (int colIdx = 0; colIdx < numberOfColumns; colIdx++) {
                  if (row == null || row.getValues() == null) {
                    builders[colIdx].append(null);
                  } else {
                    var cell = row.getValues().size() > colIdx ? row.getValues().get(colIdx) : null;
                    builders[colIdx].append(fixTypes(cell));
                  }
                }
              });
      Column[] columns = new Column[numberOfColumns];
      for (int colIdx = 0; colIdx < numberOfColumns; colIdx++) {
        columns[colIdx] = new Column(headerBuilder.get(colIdx), builders[colIdx].seal());
      }

      // Attach any problems
      return problemAggregator.attachProblemsToValue(Value.asValue(columns), reportAsErrors);
    } catch (Exception e) {
      return wrapJavaException(sheetId, e);
    }
  }

  private static Object fixTypes(CellData cell) {
    if (cell == null || cell.getEffectiveValue() == null) {
      return null;
    }

    var effectiveValue = cell.getEffectiveValue();
    var errorValue = effectiveValue.getErrorValue();
    if (errorValue != null) {
      return null;
    }

    var doubleValue = effectiveValue.getNumberValue();
    if (doubleValue == null) {
      // See if it's a boolean.
      var boolValue = effectiveValue.getBoolValue();
      if (boolValue != null) {
        return boolValue;
      }

      // Just have a text value so return it.
      return effectiveValue.getStringValue();
    }

    var format = cell.getUserEnteredFormat();
    if (format == null || format.getNumberFormat() == null) {
      if (effectiveValue.getStringValue() != null) {
        return effectiveValue.getStringValue();
      }
      return doubleValue % 1 == 0 ? doubleValue.longValue() : doubleValue;
    }

    String type = format.getNumberFormat().getType();
    switch (type) {
      case "NUMBER", "CURRENCY", "SCIENTIFIC", "PERCENT" -> {
        return effectiveValue.getNumberValue();
      }
      case "DATE" -> {
        Temporal t = fromExcelDateTime(doubleValue);
        return t instanceof java.time.LocalDateTime
            ? ((java.time.LocalDateTime) t).toLocalDate()
            : java.time.LocalDate.from(t);
      }
      case "TIME" -> {
        Temporal t = fromExcelDateTime(doubleValue);
        return t instanceof java.time.LocalDateTime
            ? ((java.time.LocalDateTime) t).toLocalTime()
            : java.time.LocalTime.from(t);
      }
      case "DATE_TIME" -> {
        Temporal t = fromExcelDateTime(doubleValue);
        return t instanceof java.time.ZonedDateTime
            ? t
            : java.time.ZonedDateTime.ofInstant(
                java.time.Instant.from(t), java.time.ZoneId.systemDefault());
      }
      case "TEXT" -> {
        return effectiveValue.getStringValue();
      }
      default -> throw new AssertionError("Unhandled format type: " + type);
    }
  }

  public Value getNumberOfSheets(String workbookId) {
    try {
      return Value.asValue(getSpreadsheetWithCache(workbookId).getSheets().size());
    } catch (Exception e) {
      return wrapJavaException(workbookId, e);
    }
  }

  public Value getSheetNames(String workbookId) {
    try {
      var output =
          getSpreadsheetWithCache(workbookId).getSheets().stream()
              .map(sheet -> sheet.getProperties().getTitle())
              .toList();
      return Value.asValue(output);
    } catch (Exception e) {
      return wrapJavaException(workbookId, e);
    }
  }

  public Value getNumberOfNames(String workbookId) {
    try {
      var namedRanges = getSpreadsheetWithCache(workbookId).getNamedRanges();
      return Value.asValue(namedRanges == null ? 0 : namedRanges.size());
    } catch (Exception e) {
      return wrapJavaException(workbookId, e);
    }
  }

  public Value getRangeNames(String workbookId) throws IOException {
    try {
      var namedRanges = getSpreadsheetWithCache(workbookId).getNamedRanges();
      var output =
          namedRanges == null
              ? List.of()
              : namedRanges.stream().map(range -> range.getName()).toList();
      return Value.asValue(output);
    } catch (Exception e) {
      return wrapJavaException(workbookId, e);
    }
  }

  private static RowData getDataRow(List<RowData> rowData, int rowIndex) {
    if (rowData.size() > rowIndex) {
      return rowData.get(rowIndex);
    }
    return null;
  }

  private static Value wrapJavaException(String workbookId, Exception exception) {
    var ensoAtom =
        Optional.ofNullable(
                switch (exception) {
                  case EmptySheetException e -> e.asEnsoValue();
                  case GoogleJsonResponseException googleJsonResponseException -> {
                    var statusCode = googleJsonResponseException.getStatusCode();
                    yield switch (statusCode) {
                      case 404 ->
                          EnsoMeta.makeInstance(
                              "Standard.Google.Google_Sheets_Workbook",
                              "Google_API_Error",
                              "Not_Found",
                              "https://docs.google.com/spreadsheets/d/"
                                  + workbookId
                                  + " was not found.");
                      case 403 ->
                          EnsoMeta.makeInstance(
                              "Standard.Google.Google_Sheets_Workbook",
                              "Google_API_Error",
                              "Access_Denied",
                              "Access to the Google Sheets document was denied. Please check your"
                                  + " permissions.");
                      default -> {
                        var details = googleJsonResponseException.getDetails();
                        var message =
                            details != null && details.getMessage() != null
                                ? details.getMessage()
                                : googleJsonResponseException.getMessage();
                        if ("This operation is not supported for this document".equals(message)) {
                          yield EnsoMeta.makeInstance(
                              "Standard.Google.Google_Sheets_Workbook",
                              "Google_API_Error",
                              "Invalid_Format",
                              "Invalid format https://docs.google.com/spreadsheets/d/"
                                  + workbookId
                                  + " is not a valid Google Sheets document.");
                        }

                        yield EnsoMeta.makeInstance(
                            "Standard.Google.Google_Sheets_Workbook",
                            "Google_API_Error",
                            "Error",
                            message,
                            googleJsonResponseException.getContent());
                      }
                    };
                  }
                  default -> null;
                })
            .or(() -> EnsoExceptionWrapper.wrapCommonExceptions(exception));
    if (ensoAtom.isEmpty()) {
      throw new RuntimeException(exception);
    }
    return EnsoMeta.asDataflowError(ensoAtom.get());
  }
}
