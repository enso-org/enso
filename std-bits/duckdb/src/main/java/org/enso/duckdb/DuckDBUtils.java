package org.enso.duckdb;

import org.duckdb.DuckDBConnection;

public class DuckDBUtils {
  /**
   * Retrieves the version of the connected DuckDB database.
   *
   * @param connection an active DuckDBConnection
   * @return the version string of the DuckDB database
   */
  public static String getVersion(DuckDBConnection connection) {
    try (var stmt = connection.createStatement();
        var rs = stmt.executeQuery("SELECT version()")) {
      if (rs.next()) {
        return rs.getString(1);
      }
      return null;
    } catch (Exception e) {
      return null;
    }
  }
}
