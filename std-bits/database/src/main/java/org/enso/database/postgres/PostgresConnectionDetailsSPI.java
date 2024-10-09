package org.enso.database.postgres;

import org.enso.database.DatabaseConnectionDetailsSPI;

public final class PostgresConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.Postgres";
  }

  @Override
  protected String getTypeName() {
    return "Postgres";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "(Postgres.Server 'localhost' 5432)";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "Postgres";
  }
}
