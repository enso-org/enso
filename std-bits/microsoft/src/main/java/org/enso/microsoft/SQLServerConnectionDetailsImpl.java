package org.enso.microsoft;

import org.enso.database.DatabaseConnectionDetailsSPI;

<<<<<<< HEAD:std-bits/microsoft/src/main/java/org/enso/microsoft/SQLServerConnectionDetailsSPI.java
public final class SQLServerConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class SQLServerConnectionDetailsImpl extends DatabaseConnectionDetailsSPI {
>>>>>>> origin/develop:std-bits/microsoft/src/main/java/org/enso/microsoft/SQLServerConnectionDetailsImpl.java

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
