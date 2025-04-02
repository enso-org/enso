package org.enso.table.read;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public final class FixedWidthFileFormatImpl extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Table.FixedWidth.FixedWidth_Format";
  }

  @Override
  protected String getTypeName() {
    return "FixedWidth_Format";
  }

  @Override
  protected String getDataLinkFormatName() {
    return "fixed_width";
  }
}
