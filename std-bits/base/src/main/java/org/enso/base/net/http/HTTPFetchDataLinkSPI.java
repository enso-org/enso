package org.enso.base.net.http;

import org.enso.base.enso_cloud.DataLinkSPI;

@org.openide.util.lookup.ServiceProvider(service = DataLinkSPI.class)
public class HTTPFetchDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Network.HTTP.HTTP_Fetch_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "HTTP_Fetch_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "HTTP";
  }
}
