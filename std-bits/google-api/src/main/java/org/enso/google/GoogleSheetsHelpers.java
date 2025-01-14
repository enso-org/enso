package org.enso.google;

import com.google.api.services.sheets.v4.Sheets;

import java.io.IOException;
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
}
