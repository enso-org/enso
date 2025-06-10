package org.enso.microsoft;

import org.enso.base.enso_cloud.DataLinkSPI;

<<<<<<< HEAD:std-bits/microsoft/src/main/java/org/enso/microsoft/SQLServerDataLinkSPI.java
public class SQLServerDataLinkSPI extends DataLinkSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = DataLinkSPI.class)
public final class SQLServerDataLinkImpl extends DataLinkSPI {
>>>>>>> origin/develop:std-bits/microsoft/src/main/java/org/enso/microsoft/SQLServerDataLinkImpl.java
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
