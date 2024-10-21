package org.enso.microsoft;

import org.enso.base.enso_cloud.DataLinkSPI;

public class SQLServerDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Microsoft.SQLServer_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "SQLServer_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "SQLServer_Connection";
  }
}
