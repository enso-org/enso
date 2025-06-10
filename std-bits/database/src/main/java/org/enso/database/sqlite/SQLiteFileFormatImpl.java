package org.enso.database.sqlite;

import org.enso.base.file_format.FileFormatSPI;

<<<<<<< HEAD:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteFormatSPI.java
public final class SQLiteFormatSPI extends FileFormatSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public final class SQLiteFileFormatImpl extends FileFormatSPI {
>>>>>>> origin/develop:std-bits/database/src/main/java/org/enso/database/sqlite/SQLiteFileFormatImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.Database.Connection.SQLite_Format";
  }

  @Override
  protected String getTypeName() {
    return "SQLite_Format";
  }
}
