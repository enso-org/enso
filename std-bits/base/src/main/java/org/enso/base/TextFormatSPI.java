package org.enso.base;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class TextFormatSPI  extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.System.File_Format";
  }

  @Override
  protected String getTypeName() {
    return "Plain_Text_Format";
  }
}
