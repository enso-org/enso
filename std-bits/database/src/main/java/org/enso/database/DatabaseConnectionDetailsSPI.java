package org.enso.database;

import java.util.ServiceLoader;
import org.enso.base.lookup.Lookup;
import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

public abstract class DatabaseConnectionDetailsSPI {
  private static final Lookup<DatabaseConnectionDetailsSPI> loader =
      Lookup.lookup((l) -> ServiceLoader.load(l, DatabaseConnectionDetailsSPI.class));

  /**
   * Returns an array of pairs, where the first element is the user facing connection name and the
   * second element is a string representing the code to insert to create a default connection
   * instance. That code may contain `_` placeholders for expected arguments.
   */
  public static String[][] get_default_constructors(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream()
        .map(
            provider -> {
              var spi = provider.get();
              return new String[] {
                spi.getUserFacingConnectionName(), spi.getCodeForDefaultConstructor()
              };
            })
        .toArray(String[][]::new);
  }

  /**
   * Returns an array of all the types that implement the `DatabaseConnectionDetailsSPI` interface.
   *
   * @param refresh whether to refresh the list of types
   * @return an array of all the types that implement the `DatabaseConnectionDetailsSPI` interface
   */
  public static Value[] get_types(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream().map(provider -> provider.get().getTypeObject()).toArray(Value[]::new);
  }

  public Value getTypeObject() {
    return EnsoMeta.getType(getModuleName(), getTypeName());
  }

  /** The module in which the connection details type is defined. */
  protected abstract String getModuleName();

  /** The name of the connection details type. */
  protected abstract String getTypeName();

  /** Default code that can be used to construct a default instance of the connection details. */
  protected abstract String getCodeForDefaultConstructor();

  /** The user facing name of the connection. */
  protected abstract String getUserFacingConnectionName();
}
