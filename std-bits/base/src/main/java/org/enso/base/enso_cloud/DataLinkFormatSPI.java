package org.enso.base.enso_cloud;

import org.enso.base.file_format.FileFormatSPI;

/** A format registration for parsing `.datalink` files as data links. */
@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class DataLinkFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Enso_Cloud.Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "Data_Link_Format";
  }
}
