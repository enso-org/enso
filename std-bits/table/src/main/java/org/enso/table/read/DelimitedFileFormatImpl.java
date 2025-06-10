package org.enso.table.read;

import org.enso.base.file_format.FileFormatSPI;

<<<<<<< HEAD:std-bits/table/src/main/java/org/enso/table/read/DelimitedFormatSPI.java
public final class DelimitedFormatSPI extends FileFormatSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public final class DelimitedFileFormatImpl extends FileFormatSPI {
>>>>>>> origin/develop:std-bits/table/src/main/java/org/enso/table/read/DelimitedFileFormatImpl.java
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
