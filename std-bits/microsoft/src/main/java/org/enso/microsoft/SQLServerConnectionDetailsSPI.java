package org.enso.microsoft;

import org.enso.database.DatabaseConnectionDetailsSPI;

public final class SQLServerConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {

  @Override
  protected String getModuleName() {
    return "Standard.Microsoft.Connection.SQLServer_Details";
  }

  @Override
  protected String getTypeName() {
    return "SQLServer_Details";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "..SQLServer";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "Microsoft SQL Server";
  }
}
