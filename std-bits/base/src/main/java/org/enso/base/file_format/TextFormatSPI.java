package org.enso.base.file_format;

public final class TextFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.System.File_Format";
  }

  @Override
  protected String getTypeName() {
    return "Plain_Text_Format";
  }
}
