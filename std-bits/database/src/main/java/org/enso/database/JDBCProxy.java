package org.enso.database;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.ServiceLoader;
import org.enso.base.enso_cloud.EnsoSecretAccessDenied;
import org.enso.base.enso_cloud.EnsoSecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import org.enso.base.lookup.Lookup;
import org.enso.database.audit.CloudAuditedConnection;
import org.enso.database.audit.LocalAuditedConnection;
import org.graalvm.collections.Pair;

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
    var drivers = Lookup.lookup((l) -> ServiceLoader.load(l, java.sql.Driver.class));
    return drivers.stream().toArray();
  }

  /**
   * Tries to create a new connection using the JDBC DriverManager.
   *
   * <p>It delegates directly to {@code DriverManager.getConnection}. That is needed because if that
   * method is called directly from Enso, the JDBC drivers are not detected correctly.
   *
   * @param url database url to connect to, starting with `jdbc:`
   * @param properties configuration for the connection
   * @return a connection
   */
  public static Connection getConnection(String url, List<Pair<String, HideableValue>> properties)
      throws SQLException {
    var drivers = Lookup.lookup((l) -> ServiceLoader.load(l, java.sql.Driver.class));

    PartitionedProperties partitionedProperties = PartitionedProperties.parse(properties);
    var rawConnection =
        EnsoSecretHelper.getJDBCConnection(drivers, url, partitionedProperties.jdbcProperties);
    return switch (partitionedProperties.audited()) {
      case "local" -> new LocalAuditedConnection(rawConnection);
      case "cloud" -> new CloudAuditedConnection(
          rawConnection, partitionedProperties.getRelatedAssetId());
      case null -> rawConnection;
      default -> throw new IllegalArgumentException(
          "Unknown audit mode: " + partitionedProperties.audited());
    };
  }

  private static final String ENSO_PROPERTY_PREFIX = "enso.internal.";
  public static final String AUDITED_KEY = ENSO_PROPERTY_PREFIX + "audit";
  public static final String RELATED_ASSET_ID_KEY = ENSO_PROPERTY_PREFIX + "relatedAssetId";

  private record PartitionedProperties(
      Map<String, String> ensoProperties, List<Pair<String, HideableValue>> jdbcProperties) {
    public static PartitionedProperties parse(List<Pair<String, HideableValue>> properties) {
      List<Pair<String, HideableValue>> jdbcProperties = new ArrayList<>();
      HashMap<String, String> ensoProperties = new HashMap<>();

      for (var pair : properties) {
        if (pair.getLeft().startsWith(ENSO_PROPERTY_PREFIX)) {
          try {
            ensoProperties.put(pair.getLeft(), pair.getRight().safeResolve());
          } catch (EnsoSecretAccessDenied e) {
            throw new IllegalStateException(
                "Internal Enso property " + pair.getLeft() + " should not contain secrets.");
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
