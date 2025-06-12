package org.enso.common;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.ServiceLoader;

/** Generic support for loading Java polyglot symbols. */
public abstract class PolyglotSymbolResolver {
  private static final Collection<PolyglotSymbolResolver> ALL;

  static {
    var arr = new ArrayList<PolyglotSymbolResolver>();
    for (var l : ServiceLoader.load(PolyglotSymbolResolver.class)) {
      arr.add(l);
    }
    ALL = Collections.unmodifiableList(arr);
  }

  /**
   * Search all providers for given name.
   *
   * @param name dot separated name to search for
   * @return non-null object representing the name
   * @throws java.lang.ClassNotFoundException if no name was found
   */
  public static Object loadClass(String name) throws ClassNotFoundException {
    var ex = new ClassNotFoundException();
    for (var p : ALL) {
      try {
        var found = p.handleLoadClass(name);
        assert found != null;
        return found;
      } catch (ClassNotFoundException cnfe) {
        ex = cnfe;
      }
    }
    throw ex;
  }

  public static void addToClassPath(URL url) {
    for (var p : ALL) {
      p.handleAddToClassPath(url);
    }
  }

  /**
   * Subclasses implement this method to search for class with the provided name.
   *
   * @param name dot separated name to search for
   * @return non-{@code null} object representing the name
   * @throws java.lang.ClassNotFoundException if no name was found
   */
  protected abstract Object handleLoadClass(String name) throws ClassNotFoundException;

  protected abstract void handleAddToClassPath(URL url);
}
