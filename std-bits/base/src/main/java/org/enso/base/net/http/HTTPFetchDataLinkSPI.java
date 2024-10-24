package org.enso.base.net.http;

import org.enso.base.enso_cloud.DataLinkSPI;

public final class HTTPFetchDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Network.HTTP.Internal.HTTP_Fetch_Data_Link";
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
