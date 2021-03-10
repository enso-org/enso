package org.enso.database;

import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverPropertyInfo;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.util.Properties;
import java.util.logging.Logger;

public class PostgresWrapper implements Driver {

  Driver underlying = new org.postgresql.Driver();

  @Override
  public Connection connect(String url, Properties info) throws SQLException {
    return underlying.connect(url, info);
  }

  @Override
  public boolean acceptsURL(String url) throws SQLException {
    return underlying.acceptsURL(url);
  }

  @Override
  public DriverPropertyInfo[] getPropertyInfo(String url, Properties info) throws SQLException {
    return underlying.getPropertyInfo(url, info);
  }

  @Override
  public int getMajorVersion() {
    return underlying.getMajorVersion();
  }

  @Override
  public int getMinorVersion() {
    return underlying.getMinorVersion();
  }

  @Override
  public boolean jdbcCompliant() {
    return underlying.jdbcCompliant();
  }

  @Override
  public Logger getParentLogger() throws SQLFeatureNotSupportedException {
    return underlying.getParentLogger();
  }
}
