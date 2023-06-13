package org.enso.database;

@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public class SQLiteConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.SQLite_Details";
  }

  @Override
  protected String getTypeName() {
    return "SQLite_Details";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "(SQLite location=_)";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "SQLite";
  }
}
