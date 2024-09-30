package org.enso.snowflake;

import org.enso.database.DatabaseConnectionDetailsSPI;

@org.openide.util.lookup.ServiceProvider(service = DatabaseConnectionDetailsSPI.class)
public class SnowflakeConnectionDetailsSPI extends DatabaseConnectionDetailsSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Snowflake.Snowflake_Details";
  }

  @Override
  protected String getTypeName() {
    return "Snowflake_Details";
  }

  @Override
  protected String getCodeForDefaultConstructor() {
    return "..Snowflake";
  }

  @Override
  protected String getUserFacingConnectionName() {
    return "Snowflake";
  }
}
