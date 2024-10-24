package org.enso.database.sqlite;

import org.enso.base.file_format.FileFormatSPI;

public final class SQLiteFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.SQLite_Format";
  }

  @Override
  protected String getTypeName() {
    return "SQLite_Format";
  }
}
