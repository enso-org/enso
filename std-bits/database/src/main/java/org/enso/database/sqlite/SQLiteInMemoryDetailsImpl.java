package org.enso.database.sqlite;

import org.enso.database.DatabaseConnectionDetailsSPI;

<<<<<<< HEAD:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteInMemoryDetailsSPI.java
public final class SQLiteInMemoryDetailsSPI extends DatabaseConnectionDetailsSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class SQLiteInMemoryDetailsImpl extends DatabaseConnectionDetailsSPI {
>>>>>>> origin/develop:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteInMemoryDetailsImpl.java
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
