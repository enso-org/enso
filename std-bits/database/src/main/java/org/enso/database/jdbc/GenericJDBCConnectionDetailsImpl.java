package org.enso.database.jdbc;

import org.enso.database.DatabaseConnectionDetailsSPI;

@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class GenericJDBCConnectionDetailsImpl extends DatabaseConnectionDetailsSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.JDBC.Generic_JDBC_Connection";
  }

  @Override
  protected String getTypeName() {
    return "Generic_JDBC_Details";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "(Generic_JDBC_Details.Value)";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "JDBC";
  }
}
