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

    GoogleSheetsHeaders columnNames =
        new GoogleSheetsHeaders(headerBehavior, rawData, problemAggregator);

    Column[] columns = new Column[rawData.size()];
    for (int i = 0; i < rawData.size(); i++) {
      var column = rawData.get(i);
      var builder = Builder.getInferredBuilder(column.size(), problemAggregator);
      column.stream().skip(columnNames.getRowsUsed()).forEach(builder::append);
      columns[i] = new Column(columnNames.get(i), builder.seal());
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
