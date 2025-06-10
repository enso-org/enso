package org.enso.aws.database;

import org.enso.database.DatabaseConnectionDetailsSPI;

<<<<<<< HEAD:std-bits/aws/src/main/java/org/enso/aws/database/RedshiftConnectionDetailsSPI.java
public final class RedshiftConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public final class RedshiftConnectionDetailsImpl extends DatabaseConnectionDetailsSPI {
>>>>>>> origin/develop:std-bits/aws/src/main/java/org/enso/aws/database/RedshiftConnectionDetailsImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.AWS.Database.Redshift.Redshift_Details";
  }

  @Override
  protected String getTypeName() {
    return "Redshift_Details";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "..Redshift";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "Redshift";
  }
}
