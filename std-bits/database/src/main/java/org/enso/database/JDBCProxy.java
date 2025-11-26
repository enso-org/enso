package org.enso.database;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import org.enso.base.enso_cloud.EnsoSecretAccessDenied;
import org.enso.base.enso_cloud.EnsoSecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.database.audit.CloudAuditedConnection;
import org.enso.database.audit.LocalAuditedConnection;

/**
 * A helper class for accessing the JDBC components.
 *
 * <p>This class is necessary because the JDBC depends on the caller's classloader to determine
 * which drivers are available and so if it is called directly from Enso it does not see the correct
 * classloaders, thus not detecting the proper drivers.
 */
public final class JDBCProxy {
  /**
   * A helper method that lists registered JDBC drivers.
   *
   * <p>Can be used for debugging.
   *
   * @return an array of JDBC drivers that are currently registered
   */
  public static Object[] getDrivers() {
    return DriverManager.drivers().toArray();
  }

  /**
   * Tries to create a new connection using the JDBC DriverManager.
   *
   * <p>It delegates directly to {@code DriverManager.getConnection}. That is needed because if that
   * method is called directly from Enso, the JDBC drivers are not detected correctly.
   *
   * @param url database url to connect to, starting with `jdbc:`
   * @param properties configuration for the connection
   * @param catalog the catalog to set on the connection, or null to not set it
   * @param schema the schema to set on the connection, or null to not set it
   * @param initScript an initialization script to run when creating a database connection
   * @return a connection
   */
  public static Connection getConnectionWithCatalogSchema(
      String url,
      List<HideableValue.KeyValuePair> properties,
      String catalog,
      String schema,
      String initScript)
      throws SQLException {
    // We need to manually register all the drivers because the DriverManager is not able
    // to correctly use our class loader, it only delegates to the platform class loader when
    // loading the java.sql.Driver service.
    var sl = ServiceLoader.load(java.sql.Driver.class, JDBCProxy.class.getClassLoader());
    for (var driver : sl) {
      DriverManager.registerDriver(driver);
    }

    PartitionedProperties partitionedProperties = PartitionedProperties.parse(properties);
    var rawConnection =
        EnsoSecretHelper.getJDBCConnection(url, partitionedProperties.jdbcProperties);

    if (catalog != null && !catalog.isEmpty()) {
      rawConnection.setCatalog(catalog);
    }
    if (schema != null && !schema.isEmpty()) {
      rawConnection.setSchema(schema);
    }

    // Run the initialization script if provided.
    if (initScript != null && !initScript.isEmpty()) {
      rawConnection.createStatement().executeUpdate(initScript);
    }

    return switch (partitionedProperties.audited()) {
      case "local" -> new LocalAuditedConnection(rawConnection);
      case "cloud" ->
          new CloudAuditedConnection(rawConnection, partitionedProperties.getRelatedAssetId());
      case null -> rawConnection;
      default ->
          throw new IllegalArgumentException(
              "Unknown audit mode: " + partitionedProperties.audited());
    };
  }

  private static final String ENSO_PROPERTY_PREFIX = "enso.internal.";
  public static final String AUDITED_KEY = ENSO_PROPERTY_PREFIX + "audit";
  public static final String RELATED_ASSET_ID_KEY = ENSO_PROPERTY_PREFIX + "relatedAssetId";

  private record PartitionedProperties(
      Map<String, String> ensoProperties, List<HideableValue.KeyValuePair> jdbcProperties) {
    public static PartitionedProperties parse(List<HideableValue.KeyValuePair> properties) {
      List<HideableValue.KeyValuePair> jdbcProperties = new ArrayList<>();
      HashMap<String, String> ensoProperties = new HashMap<>();

      for (var pair : properties) {
        if (pair.key().startsWith(ENSO_PROPERTY_PREFIX)) {
          try {
            ensoProperties.put(pair.key(), pair.value().safeResolve());
          } catch (EnsoSecretAccessDenied e) {
            throw new IllegalStateException(
                "Internal Enso property " + pair.key() + " should not contain secrets.");
          }
        } else {
          jdbcProperties.add(pair);
        }
      }

      return new PartitionedProperties(ensoProperties, jdbcProperties);
    }

    public String audited() {
      return ensoProperties.get(AUDITED_KEY);
    }

    public String getRelatedAssetId() {
      return ensoProperties.get(RELATED_ASSET_ID_KEY);
    }
  }
}
