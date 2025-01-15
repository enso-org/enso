package org.enso.google;

import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.google.api.services.sheets.v4.Sheets;
import com.google.auth.http.HttpCredentialsAdapter;
import com.google.auth.oauth2.GoogleCredentials;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.util.List;

public class GoogleSheetsHelpers {
  /*
   * We need to use this helper instead of calling the API directly from within Enso, because the intermediate values: Get request and Value implement AbstractMap which makes Enso convert them to the Enso Dictionary type and leaves us without access to their more specific methods.
   */
  public static List<List<Object>> getSheetRange(Sheets service, String sheetId, String range) throws IOException {
    return service.spreadsheets().values().get(sheetId, range)
        .setMajorDimension("COLUMNS")
        .setValueRenderOption("UNFORMATTED_VALUE")
        .execute()
        .getValues();
  }

  public static Sheets createService(GoogleCredentials credentials) throws GeneralSecurityException, IOException {
    var credentialsAdapter = new HttpCredentialsAdapter(credentials);
    Sheets.Builder builder = new Sheets.Builder(
        GoogleNetHttpTransport.newTrustedTransport(),
        GsonFactory.getDefaultInstance(),
        credentialsAdapter
    ).setApplicationName("Enso");
    return builder.build();
  }
}
