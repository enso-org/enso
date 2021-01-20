package org.enso.database.connection;

public interface Connection {
  String getDialect();
  String[] fetchColumnNamesForTable(String tableName);
}
