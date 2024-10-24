package org.enso.base.file_format;

public final class XMLFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Data.XML.XML_Format";
  }

  @Override
  protected String getTypeName() {
    return "XML_Format";
  }
}
