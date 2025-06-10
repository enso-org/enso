package org.enso.table.read;

import org.enso.base.file_format.FileFormatSPI;

<<<<<<< HEAD:std-bits/table/src/main/java/org/enso/table/read/ExcelFormatSPI.java
public final class ExcelFormatSPI extends FileFormatSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public final class ExcelFileFormatImpl extends FileFormatSPI {
>>>>>>> origin/develop:std-bits/table/src/main/java/org/enso/table/read/ExcelFileFormatImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.Table.Excel.Excel_Format";
  }

  @Override
  protected String getTypeName() {
    return "Excel_Format";
  }

  @Override
  protected String getDataLinkFormatName() {
    return "excel";
  }
}
