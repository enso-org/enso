package org.enso.database;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLTimeoutException;
import java.util.List;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.database.dryrun.OperationSynchronizer;

public final class JDBCDriverTypes {
  private final String databaseName;

  /**
   * A helper method that creates a JDBCDriverTypes record.
   *
   * @param databaseName the name of the Database type for the record
   * @return a new JDBCDriverTypes record
   */
  public static JDBCDriverTypes create(String databaseName) {
    return new JDBCDriverTypes(databaseName);
  }

  private JDBCDriverTypes(String databaseName) {
    this.databaseName = databaseName;
  }

  public String databaseName() {
    return databaseName;
  }

  public Class<SQLException> sqlExceptionClass() {
    return SQLException.class;
  }

  public Class<SQLTimeoutException> sqlTimeoutExceptionClass() {
    return SQLTimeoutException.class;
  }

  public HideableValue.Factory hideableValueFactory() {
    return new HideableValue.Factory();
  }

  public OperationSynchronizer newOperationSynchronizer() {
    return new OperationSynchronizer();
  }

  public Connection getConnectionWithCatalogSchema(
      String url, List<HideableValue.KeyValuePair> properties, String catalog, String schema)
      throws SQLException {
    return JDBCProxy.getConnectionWithCatalogSchema(url, properties, catalog, schema);
  }
}
