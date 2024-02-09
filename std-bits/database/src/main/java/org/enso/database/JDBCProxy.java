package org.enso.database;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ServiceLoader;
import org.enso.base.enso_cloud.EnsoSecretHelper;
import org.enso.base.enso_cloud.HideableValue;
import org.graalvm.collections.Pair;

/**
 * A helper class for accessing the JDBC components.
 *
 * <p>This class is necessary because the JDBC depends on the caller's classloader to determine
 * which drivers are available and so if it is called directly from Enso it does not see the correct
 * classloaders, thus not detecting the proper drivers.
 */
public class JDBCProxy {
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
   * @return a connection
   */
  public static Connection getConnection(String url, Pair<String, HideableValue>[] properties)
      throws SQLException {
    // We need to manually register all the drivers because the DriverManager is not able
    // to correctly use our class loader, it only delegates to the platform class loader when
    // loading the java.sql.Driver service.
    var sl = ServiceLoader.load(java.sql.Driver.class, JDBCProxy.class.getClassLoader());
    for (var driver : sl) {
      DriverManager.registerDriver(driver);
    }

    return EnsoSecretHelper.getJDBCConnection(url, properties);
  }
}
