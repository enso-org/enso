package org.enso.database.sqlite;

import org.enso.database.DatabaseConnectionDetailsSPI;

public final class SQLiteInMemoryDetailsSPI extends DatabaseConnectionDetailsSPI {
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
    return "SQLite.In_Memory";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "SQLite (In-Memory)";
  }
}
