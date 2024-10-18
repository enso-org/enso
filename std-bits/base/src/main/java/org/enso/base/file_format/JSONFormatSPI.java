package org.enso.base.file_format;

public final class JSONFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.System.File_Format";
  }

  @Override
  protected String getTypeName() {
    return "JSON_Format";
  }

  @Override
  protected String getDataLinkFormatName() {
    return "json";
  }
}
