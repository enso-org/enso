package org.enso.database;

<<<<<<< HEAD
import java.util.ServiceLoader;
import org.enso.base.lookup.Lookup;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public abstract class DatabaseConnectionDetailsSPI {
  private static final Lookup<DatabaseConnectionDetailsSPI> loader =
      Lookup.lookup((l) -> ServiceLoader.load(l, DatabaseConnectionDetailsSPI.class));
=======
import java.util.List;
import org.enso.base.spi.EnsoService;
import org.enso.base.spi.EnsoServiceLoader;
import org.graalvm.polyglot.Value;

public abstract class DatabaseConnectionDetailsSPI extends EnsoService {
  private static final EnsoServiceLoader<DatabaseConnectionDetailsSPI> loader =
      EnsoServiceLoader.load(DatabaseConnectionDetailsSPI.class);
>>>>>>> origin/develop

  /**
   * Returns an array of pairs, where the first element is the user facing connection name and the
   * second element is a string representing the code to insert to create a default connection
   * instance. That code may contain `_` placeholders for expected arguments.
   */
  public static String[][] get_default_constructors(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.getProviders().stream()
        .map(
            provider ->
                new String[] {
                  provider.getUserFacingConnectionName(), provider.getCodeForDefaultConstructor()
                })
        .toArray(String[][]::new);
  }

  /**
   * Returns an array of all the types that implement the `DatabaseConnectionDetailsSPI` interface.
   *
   * @param refresh whether to refresh the list of types
   * @return a list of all the types that implement the `DatabaseConnectionDetailsSPI` interface
   */
  public static List<Value> get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.getTypeObjects();
  }

  /** Default code that can be used to construct a default instance of the connection details. */
  protected abstract String getCodeForDefaultConstructor();

  /** The user facing name of the connection. */
  protected abstract String getUserFacingConnectionName();
}
