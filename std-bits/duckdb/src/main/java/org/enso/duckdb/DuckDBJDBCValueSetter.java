package org.enso.duckdb;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.time.LocalDate;
import java.time.LocalTime;
import org.enso.database.JDBCValueSetter;

public class DuckDBJDBCValueSetter extends JDBCValueSetter {
  public DuckDBJDBCValueSetter() {
    super("DuckDB");
  }

  @Override
  public void setLocalTime(PreparedStatement stmt, int columnIndex, LocalTime localTime)
      throws SQLException {
    stmt.setObject(columnIndex, localTime);
  }

  @Override
  public void setLocalDate(PreparedStatement stmt, int columnIndex, LocalDate localDate)
      throws SQLException {
    stmt.setObject(columnIndex, localDate);
  }
}
