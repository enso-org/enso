package org.enso.database;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class EnsoConnectionSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Enso_Connection";
  }

  @Override
  protected String getTypeName() {
    return "Enso_Connection";
  }
}
