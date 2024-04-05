package org.enso.snowflake;

import org.enso.base.enso_cloud.DataLinkSPI;

@org.openide.util.lookup.ServiceProvider(service = DataLinkSPI.class)
public class SnowflakeDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Snowflake.Snowflake_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "Snowflake_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "Snowflake_Connection";
  }
}
