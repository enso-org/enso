package org.enso.database.connection;

import java.util.HashMap;
import java.util.Map;

public class FakeTestConnection implements Connection {
  private Map<String, String[]> tables;

  public FakeTestConnection() {
    tables = new HashMap<>();
  }

  @Override
  public String[] fetchColumnNamesForTable(String tableName) {
    return tables.get(tableName);
  }

  public void addTable(String name, String[] columns) {
    tables.put(name, columns);
  }
}
