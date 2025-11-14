package org.enso.duckdb;

import org.enso.database.JDBCValueSetter;

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;
import java.time.LocalDate;
import java.time.LocalTime;

public class DuckDBJDBCValueSetter extends JDBCValueSetter {
    public DuckDBJDBCValueSetter() {
        super("DuckDB");
    }

    @Override
    public void setLocalTime(PreparedStatement stmt, int columnIndex, LocalTime localTime) throws SQLException {
        stmt.setObject(columnIndex, localTime);
    }

    @Override
    public void setLocalDate(PreparedStatement stmt, int columnIndex, LocalDate localDate) throws SQLException {
        stmt.setObject(columnIndex, localDate);
    }
}
