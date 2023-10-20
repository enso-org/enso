package org.enso.database;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class TestFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.Connection";
  }

  @Override
  protected String getTypeName() {
    return "Connection";
  }
}
