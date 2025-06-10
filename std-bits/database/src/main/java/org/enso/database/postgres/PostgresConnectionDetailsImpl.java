package org.enso.database.postgres;

import org.enso.database.DatabaseConnectionDetailsSPI;

<<<<<<< HEAD:std-bits/database/src/main/java/org/enso/database/postgres/PostgresConnectionDetailsSPI.java
public final class PostgresConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class PostgresConnectionDetailsImpl extends DatabaseConnectionDetailsSPI {
>>>>>>> origin/develop:std-bits/database/src/main/java/org/enso/database/postgres/PostgresConnectionDetailsImpl.java
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
