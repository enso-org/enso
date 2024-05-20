package org.enso.database.audit;

import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.net.URL;
import java.sql.Array;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.Date;
import java.sql.NClob;
import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.Ref;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.RowId;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.Calendar;

abstract class AuditedPreparedStatement implements PreparedStatement {
  private final PreparedStatement underlying;
  private final String sql;

  protected abstract void auditQuery(String operationType, String sql);

  public AuditedPreparedStatement(PreparedStatement underlying, String sql) {
    this.underlying = underlying;
    this.sql = sql;
  }

  @Override
  public ResultSet executeQuery() throws SQLException {
    auditQuery("query", sql);
    return underlying.executeQuery();
  }

  @Override
  public int executeUpdate() throws SQLException {
    auditQuery("update", sql);
    return underlying.executeUpdate();
  }

  @Override
  public void setNull(int parameterIndex, int sqlType) throws SQLException {
    underlying.setNull(parameterIndex, sqlType);
  }

  @Override
  public void setBoolean(int parameterIndex, boolean x) throws SQLException {
    underlying.setBoolean(parameterIndex, x);
  }

  @Override
  public void setByte(int parameterIndex, byte x) throws SQLException {
    underlying.setByte(parameterIndex, x);
  }

  @Override
  public void setShort(int parameterIndex, short x) throws SQLException {
    underlying.setShort(parameterIndex, x);
  }

  @Override
  public void setInt(int parameterIndex, int x) throws SQLException {
    underlying.setInt(parameterIndex, x);
  }

  @Override
  public void setLong(int parameterIndex, long x) throws SQLException {
    underlying.setLong(parameterIndex, x);
  }

  @Override
  public void setFloat(int parameterIndex, float x) throws SQLException {
    underlying.setFloat(parameterIndex, x);
  }

  @Override
  public void setDouble(int parameterIndex, double x) throws SQLException {
    underlying.setDouble(parameterIndex, x);
  }

  @Override
  public void setBigDecimal(int parameterIndex, BigDecimal x) throws SQLException {
    underlying.setBigDecimal(parameterIndex, x);
  }

  @Override
  public void setString(int parameterIndex, String x) throws SQLException {
    underlying.setString(parameterIndex, x);
  }

  @Override
  public void setBytes(int parameterIndex, byte[] x) throws SQLException {
    underlying.setBytes(parameterIndex, x);
  }

  @Override
  public void setDate(int parameterIndex, Date x) throws SQLException {
    underlying.setDate(parameterIndex, x);
  }

  @Override
  public void setTime(int parameterIndex, Time x) throws SQLException {
    underlying.setTime(parameterIndex, x);
  }

  @Override
  public void setTimestamp(int parameterIndex, Timestamp x) throws SQLException {
    underlying.setTimestamp(parameterIndex, x);
  }

  @Override
  public void setAsciiStream(int parameterIndex, InputStream x, int length) throws SQLException {
    underlying.setAsciiStream(parameterIndex, x);
  }

  @SuppressWarnings("deprecation")
  @Override
  public void setUnicodeStream(int parameterIndex, InputStream x, int length) throws SQLException {
    underlying.setUnicodeStream(parameterIndex, x, length);
  }

  @Override
  public void setBinaryStream(int parameterIndex, InputStream x, int length) throws SQLException {
    underlying.setBinaryStream(parameterIndex, x, length);
  }

  @Override
  public void clearParameters() throws SQLException {
    underlying.clearParameters();
  }

  @Override
  public void setObject(int parameterIndex, Object x, int targetSqlType) throws SQLException {
    underlying.setObject(parameterIndex, x, targetSqlType);
  }

  @Override
  public void setObject(int parameterIndex, Object x) throws SQLException {
    underlying.setObject(parameterIndex, x);
  }

  @Override
  public boolean execute() throws SQLException {
    auditQuery("execute", sql);
    return underlying.execute();
  }

  @Override
  public void addBatch() throws SQLException {
    underlying.addBatch();
  }

  @Override
  public void setCharacterStream(int parameterIndex, Reader reader, int length)
      throws SQLException {
    underlying.setCharacterStream(parameterIndex, reader, length);
  }

  @Override
  public void setRef(int parameterIndex, Ref x) throws SQLException {
    underlying.setRef(parameterIndex, x);
  }

  @Override
  public void setBlob(int parameterIndex, Blob x) throws SQLException {
    underlying.setBlob(parameterIndex, x);
  }

  @Override
  public void setClob(int parameterIndex, Clob x) throws SQLException {
    underlying.setClob(parameterIndex, x);
  }

  @Override
  public void setArray(int parameterIndex, Array x) throws SQLException {
    underlying.setArray(parameterIndex, x);
  }

  @Override
  public ResultSetMetaData getMetaData() throws SQLException {
    return underlying.getMetaData();
  }

  @Override
  public void setDate(int parameterIndex, Date x, Calendar cal) throws SQLException {
    underlying.setDate(parameterIndex, x, cal);
  }

  @Override
  public void setTime(int parameterIndex, Time x, Calendar cal) throws SQLException {
    underlying.setTime(parameterIndex, x, cal);
  }

  @Override
  public void setTimestamp(int parameterIndex, Timestamp x, Calendar cal) throws SQLException {
    underlying.setTimestamp(parameterIndex, x, cal);
  }

  @Override
  public void setNull(int parameterIndex, int sqlType, String typeName) throws SQLException {
    underlying.setNull(parameterIndex, sqlType, typeName);
  }

  @Override
  public void setURL(int parameterIndex, URL x) throws SQLException {
    underlying.setURL(parameterIndex, x);
  }

  @Override
  public ParameterMetaData getParameterMetaData() throws SQLException {
    return underlying.getParameterMetaData();
  }

  @Override
  public void setRowId(int parameterIndex, RowId x) throws SQLException {
    underlying.setRowId(parameterIndex, x);
  }

  @Override
  public void setNString(int parameterIndex, String value) throws SQLException {
    underlying.setNString(parameterIndex, value);
  }

  @Override
  public void setNCharacterStream(int parameterIndex, Reader value, long length)
      throws SQLException {
    underlying.setNCharacterStream(parameterIndex, value, length);
  }

  @Override
  public void setNClob(int parameterIndex, NClob value) throws SQLException {
    underlying.setNClob(parameterIndex, value);
  }

  @Override
  public void setClob(int parameterIndex, Reader reader, long length) throws SQLException {
    underlying.setClob(parameterIndex, reader, length);
  }

  @Override
  public void setBlob(int parameterIndex, InputStream inputStream, long length)
      throws SQLException {
    underlying.setBlob(parameterIndex, inputStream, length);
  }

  @Override
  public void setNClob(int parameterIndex, Reader reader, long length) throws SQLException {
    underlying.setNClob(parameterIndex, reader, length);
  }

  @Override
  public void setSQLXML(int parameterIndex, SQLXML xmlObject) throws SQLException {
    underlying.setSQLXML(parameterIndex, xmlObject);
  }

  @Override
  public void setObject(int parameterIndex, Object x, int targetSqlType, int scaleOrLength)
      throws SQLException {
    underlying.setObject(parameterIndex, x, targetSqlType, scaleOrLength);
  }

  @Override
  public void setAsciiStream(int parameterIndex, InputStream x, long length) throws SQLException {
    underlying.setAsciiStream(parameterIndex, x, length);
  }

  @Override
  public void setBinaryStream(int parameterIndex, InputStream x, long length) throws SQLException {
    underlying.setBinaryStream(parameterIndex, x, length);
  }

  @Override
  public void setCharacterStream(int parameterIndex, Reader reader, long length)
      throws SQLException {
    underlying.setCharacterStream(parameterIndex, reader, length);
  }

  @Override
  public void setAsciiStream(int parameterIndex, InputStream x) throws SQLException {
    underlying.setAsciiStream(parameterIndex, x);
  }

  @Override
  public void setBinaryStream(int parameterIndex, InputStream x) throws SQLException {
    underlying.setBinaryStream(parameterIndex, x);
  }

  @Override
  public void setCharacterStream(int parameterIndex, Reader reader) throws SQLException {
    underlying.setCharacterStream(parameterIndex, reader);
  }

  @Override
  public void setNCharacterStream(int parameterIndex, Reader value) throws SQLException {
    underlying.setNCharacterStream(parameterIndex, value);
  }

  @Override
  public void setClob(int parameterIndex, Reader reader) throws SQLException {
    underlying.setClob(parameterIndex, reader);
  }

  @Override
  public void setBlob(int parameterIndex, InputStream inputStream) throws SQLException {
    underlying.setBlob(parameterIndex, inputStream);
  }

  @Override
  public void setNClob(int parameterIndex, Reader reader) throws SQLException {
    underlying.setNClob(parameterIndex, reader);
  }

  @Override
  public ResultSet executeQuery(String sql) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public int executeUpdate(String sql) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public void close() throws SQLException {
    underlying.close();
  }

  @Override
  public int getMaxFieldSize() throws SQLException {
    return underlying.getMaxFieldSize();
  }

  @Override
  public void setMaxFieldSize(int max) throws SQLException {
    underlying.setMaxFieldSize(max);
  }

  @Override
  public int getMaxRows() throws SQLException {
    return underlying.getMaxRows();
  }

  @Override
  public void setMaxRows(int max) throws SQLException {
    underlying.setMaxRows(max);
  }

  @Override
  public void setEscapeProcessing(boolean enable) throws SQLException {
    underlying.setEscapeProcessing(enable);
  }

  @Override
  public int getQueryTimeout() throws SQLException {
    return underlying.getQueryTimeout();
  }

  @Override
  public void setQueryTimeout(int seconds) throws SQLException {
    underlying.setQueryTimeout(seconds);
  }

  @Override
  public void cancel() throws SQLException {
    underlying.cancel();
  }

  @Override
  public SQLWarning getWarnings() throws SQLException {
    return underlying.getWarnings();
  }

  @Override
  public void clearWarnings() throws SQLException {
    underlying.clearWarnings();
  }

  @Override
  public void setCursorName(String name) throws SQLException {
    underlying.setCursorName(name);
  }

  @Override
  public boolean execute(String sql) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public ResultSet getResultSet() throws SQLException {
    return underlying.getResultSet();
  }

  @Override
  public int getUpdateCount() throws SQLException {
    return underlying.getUpdateCount();
  }

  @Override
  public boolean getMoreResults() throws SQLException {
    return underlying.getMoreResults();
  }

  @Override
  public int getFetchDirection() throws SQLException {
    return underlying.getFetchDirection();
  }

  @Override
  public void setFetchDirection(int direction) throws SQLException {
    underlying.setFetchDirection(direction);
  }

  @Override
  public int getFetchSize() throws SQLException {
    return underlying.getFetchSize();
  }

  @Override
  public void setFetchSize(int rows) throws SQLException {
    underlying.setFetchSize(rows);
  }

  @Override
  public int getResultSetConcurrency() throws SQLException {
    return underlying.getResultSetConcurrency();
  }

  @Override
  public int getResultSetType() throws SQLException {
    return underlying.getResultSetType();
  }

  @Override
  public void addBatch(String sql) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public void clearBatch() throws SQLException {
    underlying.clearBatch();
  }

  private boolean firstInBatch = true;

  @Override
  public int[] executeBatch() throws SQLException {
    if (firstInBatch) {
      auditQuery("executeBatch", sql);
      firstInBatch = false;
    }
    return underlying.executeBatch();
  }

  @Override
  public Connection getConnection() throws SQLException {
    throw new IllegalStateException("`getConnection` cannot be called on AuditedPreparedStatement");
  }

  @Override
  public boolean getMoreResults(int current) throws SQLException {
    return underlying.getMoreResults(current);
  }

  @Override
  public ResultSet getGeneratedKeys() throws SQLException {
    return underlying.getGeneratedKeys();
  }

  @Override
  public int executeUpdate(String sql, int autoGeneratedKeys) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public int executeUpdate(String sql, int[] columnIndexes) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public int executeUpdate(String sql, String[] columnNames) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public boolean execute(String sql, int autoGeneratedKeys) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public boolean execute(String sql, int[] columnIndexes) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public boolean execute(String sql, String[] columnNames) throws SQLException {
    throw new IllegalStateException("Cannot be called on PreparedStatement");
  }

  @Override
  public int getResultSetHoldability() throws SQLException {
    return underlying.getResultSetHoldability();
  }

  @Override
  public boolean isClosed() throws SQLException {
    return underlying.isClosed();
  }

  @Override
  public boolean isPoolable() throws SQLException {
    return underlying.isPoolable();
  }

  @Override
  public void setPoolable(boolean poolable) throws SQLException {
    underlying.setPoolable(poolable);
  }

  @Override
  public void closeOnCompletion() throws SQLException {
    underlying.closeOnCompletion();
  }

  @Override
  public boolean isCloseOnCompletion() throws SQLException {
    return underlying.isCloseOnCompletion();
  }

  @Override
  public <T> T unwrap(Class<T> iface) throws SQLException {
    return underlying.unwrap(iface);
  }

  @Override
  public boolean isWrapperFor(Class<?> iface) throws SQLException {
    return underlying.isWrapperFor(iface);
  }
}
