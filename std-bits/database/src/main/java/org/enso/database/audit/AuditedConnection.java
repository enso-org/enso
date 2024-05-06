package org.enso.database.audit;

import java.sql.Array;
import java.sql.Blob;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.NClob;
import java.sql.PreparedStatement;
import java.sql.SQLClientInfoException;
import java.sql.SQLException;
import java.sql.SQLWarning;
import java.sql.SQLXML;
import java.sql.Savepoint;
import java.sql.Statement;
import java.sql.Struct;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executor;

abstract class AuditedConnection implements Connection {
  protected final Connection underlying;

  AuditedConnection(Connection underlying) {
    this.underlying = underlying;
  }

  abstract void auditQuery(String operationType, String sql);

  abstract void auditTransaction(String operation);

  private RuntimeException unimplemented(String name) {
    throw new UnsupportedOperationException(
        name + " is not implemented. This is a bug in the Database library.");
  }

  @Override
  public Statement createStatement() throws SQLException {
    return new AuditedStatementImpl(underlying.createStatement());
  }

  @Override
  public PreparedStatement prepareStatement(String sql) throws SQLException {
    return new AuditedPreparedStatementImpl(underlying.prepareStatement(sql), sql);
  }

  @Override
  public CallableStatement prepareCall(String sql) throws SQLException {
    throw unimplemented("prepareCall");
  }

  @Override
  public String nativeSQL(String sql) throws SQLException {
    return underlying.nativeSQL(sql);
  }

  @Override
  public void setAutoCommit(boolean autoCommit) throws SQLException {
    if (autoCommit != underlying.getAutoCommit()) {
      auditTransaction("setAutoCommit " + autoCommit);
    }
    underlying.setAutoCommit(autoCommit);
  }

  @Override
  public boolean getAutoCommit() throws SQLException {
    return underlying.getAutoCommit();
  }

  @Override
  public void commit() throws SQLException {
    auditTransaction("commit");
    underlying.commit();
  }

  @Override
  public void rollback() throws SQLException {
    auditTransaction("rollback");
    underlying.rollback();
  }

  @Override
  public void close() throws SQLException {
    underlying.close();
  }

  @Override
  public boolean isClosed() throws SQLException {
    return underlying.isClosed();
  }

  @Override
  public DatabaseMetaData getMetaData() throws SQLException {
    return underlying.getMetaData();
  }

  @Override
  public void setReadOnly(boolean readOnly) throws SQLException {
    underlying.setReadOnly(readOnly);
  }

  @Override
  public boolean isReadOnly() throws SQLException {
    return underlying.isReadOnly();
  }

  @Override
  public void setCatalog(String catalog) throws SQLException {
    underlying.setCatalog(catalog);
  }

  @Override
  public String getCatalog() throws SQLException {
    return underlying.getCatalog();
  }

  @Override
  public void setTransactionIsolation(int level) throws SQLException {
    underlying.setTransactionIsolation(level);
  }

  @Override
  public int getTransactionIsolation() throws SQLException {
    return underlying.getTransactionIsolation();
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
  public Statement createStatement(int resultSetType, int resultSetConcurrency)
      throws SQLException {
    return new AuditedStatementImpl(
        underlying.createStatement(resultSetType, resultSetConcurrency));
  }

  @Override
  public PreparedStatement prepareStatement(String sql, int resultSetType, int resultSetConcurrency)
      throws SQLException {
    return new AuditedPreparedStatementImpl(
        underlying.prepareStatement(sql, resultSetType, resultSetConcurrency), sql);
  }

  @Override
  public CallableStatement prepareCall(String sql, int resultSetType, int resultSetConcurrency)
      throws SQLException {
    throw unimplemented("prepareCall");
  }

  @Override
  public Map<String, Class<?>> getTypeMap() throws SQLException {
    return underlying.getTypeMap();
  }

  @Override
  public void setTypeMap(Map<String, Class<?>> map) throws SQLException {
    underlying.setTypeMap(map);
  }

  @Override
  public void setHoldability(int holdability) throws SQLException {
    underlying.setHoldability(holdability);
  }

  @Override
  public int getHoldability() throws SQLException {
    return underlying.getHoldability();
  }

  private String savePointToString(Savepoint savepoint) {
    try {
      return savepoint.getSavepointName();
    } catch (SQLException e) {
      // Do nothing
    }

    try {
      return "Savepoint " + savepoint.getSavepointName();
    } catch (SQLException e) {
      return savepoint.toString();
    }
  }

  @Override
  public Savepoint setSavepoint() throws SQLException {
    var savepoint = underlying.setSavepoint();
    auditTransaction("setSavepoint " + savePointToString(savepoint));
    return savepoint;
  }

  @Override
  public Savepoint setSavepoint(String name) throws SQLException {
    var savepoint = underlying.setSavepoint(name);
    auditTransaction("setSavepoint " + savePointToString(savepoint));
    return savepoint;
  }

  @Override
  public void rollback(Savepoint savepoint) throws SQLException {
    auditTransaction("rollback " + savePointToString(savepoint));
    underlying.rollback(savepoint);
  }

  @Override
  public void releaseSavepoint(Savepoint savepoint) throws SQLException {
    auditTransaction("releaseSavepoint " + savePointToString(savepoint));
    underlying.releaseSavepoint(savepoint);
  }

  @Override
  public Statement createStatement(
      int resultSetType, int resultSetConcurrency, int resultSetHoldability) throws SQLException {
    return new AuditedStatementImpl(
        underlying.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability));
  }

  @Override
  public PreparedStatement prepareStatement(
      String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability)
      throws SQLException {
    return new AuditedPreparedStatementImpl(
        underlying.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability),
        sql);
  }

  @Override
  public CallableStatement prepareCall(
      String sql, int resultSetType, int resultSetConcurrency, int resultSetHoldability)
      throws SQLException {
    throw unimplemented("prepareCall");
  }

  @Override
  public PreparedStatement prepareStatement(String sql, int autoGeneratedKeys) throws SQLException {
    return new AuditedPreparedStatementImpl(
        underlying.prepareStatement(sql, autoGeneratedKeys), sql);
  }

  @Override
  public PreparedStatement prepareStatement(String sql, int[] columnIndexes) throws SQLException {
    return new AuditedPreparedStatementImpl(underlying.prepareStatement(sql, columnIndexes), sql);
  }

  @Override
  public PreparedStatement prepareStatement(String sql, String[] columnNames) throws SQLException {
    return new AuditedPreparedStatementImpl(underlying.prepareStatement(sql, columnNames), sql);
  }

  @Override
  public Clob createClob() throws SQLException {
    return underlying.createClob();
  }

  @Override
  public Blob createBlob() throws SQLException {
    return underlying.createBlob();
  }

  @Override
  public NClob createNClob() throws SQLException {
    return underlying.createNClob();
  }

  @Override
  public SQLXML createSQLXML() throws SQLException {
    return underlying.createSQLXML();
  }

  @Override
  public boolean isValid(int timeout) throws SQLException {
    return underlying.isValid(timeout);
  }

  @Override
  public void setClientInfo(String name, String value) throws SQLClientInfoException {
    underlying.setClientInfo(name, value);
  }

  @Override
  public void setClientInfo(Properties properties) throws SQLClientInfoException {
    underlying.setClientInfo(properties);
  }

  @Override
  public String getClientInfo(String name) throws SQLException {
    return underlying.getClientInfo(name);
  }

  @Override
  public Properties getClientInfo() throws SQLException {
    return underlying.getClientInfo();
  }

  @Override
  public Array createArrayOf(String typeName, Object[] elements) throws SQLException {
    return underlying.createArrayOf(typeName, elements);
  }

  @Override
  public Struct createStruct(String typeName, Object[] attributes) throws SQLException {
    return underlying.createStruct(typeName, attributes);
  }

  @Override
  public void setSchema(String schema) throws SQLException {
    underlying.setSchema(schema);
  }

  @Override
  public String getSchema() throws SQLException {
    return underlying.getSchema();
  }

  @Override
  public void abort(Executor executor) throws SQLException {
    auditTransaction("abort");
    underlying.abort(executor);
  }

  @Override
  public void setNetworkTimeout(Executor executor, int milliseconds) throws SQLException {
    underlying.setNetworkTimeout(executor, milliseconds);
  }

  @Override
  public int getNetworkTimeout() throws SQLException {
    return underlying.getNetworkTimeout();
  }

  @Override
  public <T> T unwrap(Class<T> iface) throws SQLException {
    return underlying.unwrap(iface);
  }

  @Override
  public boolean isWrapperFor(Class<?> iface) throws SQLException {
    return underlying.isWrapperFor(iface);
  }

  private class AuditedPreparedStatementImpl extends AuditedPreparedStatement {
    public AuditedPreparedStatementImpl(PreparedStatement underlying, String sql) {
      super(underlying, sql);
    }

    @Override
    protected void auditQuery(String operationType, String sql) {
      AuditedConnection.this.auditQuery(operationType, sql);
    }
  }

  private class AuditedStatementImpl extends AuditedStatement {
    public AuditedStatementImpl(Statement underlying) {
      super(underlying);
    }

    @Override
    protected void auditQuery(String operationType, String sql) {
      AuditedConnection.this.auditQuery(operationType, sql);
    }
  }
}
