package org.enso.database.connection;

public interface Connection {
  String[] fetchColumnNamesForTable(String tableName);
}
