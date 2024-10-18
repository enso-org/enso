package org.enso.base.enso_cloud;

public final class EnsoFileDataLinkSPI extends DataLinkSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Enso_Cloud.Internal.Enso_File_Data_Link";
  }

  @Override
  protected String getTypeName() {
    return "Enso_File_Data_Link";
  }

  @Override
  protected String getLinkTypeName() {
    return "Enso_File";
  }
}
