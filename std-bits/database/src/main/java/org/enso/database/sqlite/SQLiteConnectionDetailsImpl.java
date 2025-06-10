package org.enso.database.sqlite;

import org.enso.database.DatabaseConnectionDetailsSPI;

<<<<<<< HEAD:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteConnectionDetailsSPI.java
public final class SQLiteConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class SQLiteConnectionDetailsImpl extends DatabaseConnectionDetailsSPI {
>>>>>>> origin/develop:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteConnectionDetailsImpl.java
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
