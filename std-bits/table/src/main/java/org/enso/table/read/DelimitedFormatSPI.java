package org.enso.table.read;

import org.enso.base.file_format.FileFormatSPI;

public final class DelimitedFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Table.Delimited.Delimited_Format";
  }

  @Override
  protected String getTypeName() {
    return "Delimited_Format";
  }

  @Override
  protected String getDataLinkFormatName() {
    return "delimited";
  }
}
