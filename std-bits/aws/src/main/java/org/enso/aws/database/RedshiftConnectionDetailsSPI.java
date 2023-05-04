package org.enso.aws.database;

import org.enso.database.DatabaseConnectionDetailsSPI;

@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public class RedshiftConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
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
    return getModuleName() + "." + getTypeName() + ".Redshift";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "Redshift";
  }
}
