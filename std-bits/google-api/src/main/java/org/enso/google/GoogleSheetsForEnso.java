package org.enso.google;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.SheetsScopes;
import com.google.auth.http.HttpCredentialsAdapter;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.EmptySheetException;
import org.enso.table.problems.ProblemAggregator;

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

    List<Object> firstRow = null;
    if (rawData.stream().anyMatch(col -> col.size() > 0)) {
      firstRow = rawData.stream().map(col -> col.size() > 0 ? col.get(0) : null).toList();
    }

    List<Object> secondRow = null;
    if (rawData.stream().anyMatch(col -> col.size() > 1)) {
      secondRow = rawData.stream().map(col -> col.size() > 1 ? col.get(1) : null).toList();
    }

    GoogleSheetsHeaders headerBuilder =
        new GoogleSheetsHeaders(headerBehavior, firstRow, secondRow, problemAggregator);

    Column[] columns = new Column[rawData.size()];
    var resolved_row_limit = row_limit == null ? Long.MAX_VALUE : (row_limit < 0 ? 0 : row_limit);
    for (int i = 0; i < rawData.size(); i++) {
      var column = rawData.get(i);
      var builder = Builder.getInferredBuilder(column.size(), problemAggregator);
      column.stream()
          .skip(skip_rows)
          .skip(headerBuilder.getRowsUsed())
          .limit(resolved_row_limit)
          .forEach(builder::append);
      columns[i] = new Column(headerBuilder.get(i), builder.seal());
    }
    return new Table(columns);
  }

  public List<String> getSheetNames(String workbookId) throws IOException {
    return service
        .spreadsheets()
        .get(workbookId)
        .setIncludeGridData(false)
        .execute()
        .getSheets()
        .stream()
        .map(sheet -> sheet.getProperties().getTitle())
        .toList();
  }

  public int getNumberOfSheets(String workbookId) throws IOException {
    return service
        .spreadsheets()
        .get(workbookId)
        .setIncludeGridData(false)
        .execute()
        .getSheets()
        .size();
  }
}
