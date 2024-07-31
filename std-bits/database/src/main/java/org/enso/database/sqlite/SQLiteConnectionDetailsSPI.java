package org.enso.database.sqlite;

import org.enso.database.DatabaseConnectionDetailsSPI;

public final class SQLiteConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.SQLite";
  }

  @Override
  protected String getTypeName() {
    return "SQLite";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "SQLite.From_File";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "SQLite";
  }
}
