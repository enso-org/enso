package org.enso.database;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.ServiceLoader;

public abstract class DatabaseConnectionDetailsSPI { private static final ServiceLoader<DatabaseConnectionDetailsSPI> loader =
      ServiceLoader.load(DatabaseConnectionDetailsSPI.class, DatabaseConnectionDetailsSPI.class.getClassLoader());

  /** Returns an array of pairs, where the first element is the user facing connection name and the second element is a string representing the code to insert to create a default connection instance. That code may contain `_` placeholders for expected arguments. */
  public static String[][] get_default_constructors(boolean refresh) {
    if (refresh) {
      loader.reload();
    }
    return loader.stream().map(provider -> {
      var spi = provider.get();
      return new String[] {spi.getUserFacingConnectionName(), spi.getCodeForDefaultConstructor()};
    }).toArray(String[][]::new);
  }

  public Value makeDefaultWidgetOption() {
    final var context = Context.getCurrent().getBindings("enso");
    final String optionModuleName = "Standard.Base.Metadata";
    final var module = context.invokeMember("get_module", optionModuleName);
    final var option_type = module.invokeMember("get_type", "Choice");
    final var option_ctor = module.invokeMember("get_method", option_type, "Option");

    String label = getUserFacingConnectionName();
    String value = getModuleName() + "." + getTypeName() + "." + getCodeForDefaultConstructor();
    return option_ctor.execute(label, value);
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
