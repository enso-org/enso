package org.enso.database;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.SQLTimeoutException;
import java.util.List;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.database.dryrun.OperationSynchronizer;

public final class JDBCDriverTypes {
  private final String databaseName;
  private final String initScript;

  /**
   * A helper method that creates a JDBCDriverTypes record.
   *
   * @param databaseName the name of the Database type for the record
   * @return a new JDBCDriverTypes record
   */
  public static JDBCDriverTypes create(String databaseName) {
    return JDBCDriverTypes.createWithInitScript(databaseName, "");
  }

  /**
   * A helper method that creates a JDBCDriverTypes record.
   *
   * @param databaseName the name of the Database type for the record
   * @param initScript an initialization script to run when creating a database connection
   * @return a new JDBCDriverTypes record
   */
  public static JDBCDriverTypes createWithInitScript(String databaseName, String initScript) {
    return new JDBCDriverTypes(databaseName, initScript);
  }

  private JDBCDriverTypes(String databaseName, String initScript) {
    this.databaseName = databaseName;
    this.initScript = initScript;
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

  public Class<UnsupportedOperationException> unsupportedExceptionClass() {
    return UnsupportedOperationException.class;
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
    return JDBCProxy.getConnectionWithCatalogSchema(url, properties, catalog, schema, initScript);
  }
}
