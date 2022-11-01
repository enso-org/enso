package org.enso.table.read;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class ExcelFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Table.Excel.Excel_Format";
  }

  @Override
  protected String getTypeName() {
    return "Excel_Format";
  }
}
