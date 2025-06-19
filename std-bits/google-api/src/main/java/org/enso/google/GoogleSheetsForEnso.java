package org.enso.google;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.api.services.sheets.v4.SheetsScopes;
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

  public Table getSheetRange(String sheetId, String range) throws IOException {
    var data = service
        .spreadsheets()
        .values()
        .get(sheetId, range)
        .setMajorDimension("COLUMNS")
        .setValueRenderOption("UNFORMATTED_VALUE")
        .execute()
        .getValues();

    var columns = data.stream()
        .map(column -> {
            var builder = Builder.getForText(TextType.VARIABLE_LENGTH, column.size());
            column.stream().skip(1).forEach(builder::append);
            return new Column(column.get(0).toString(), builder.seal());
        })
        .toArray(Column[]::new);
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
