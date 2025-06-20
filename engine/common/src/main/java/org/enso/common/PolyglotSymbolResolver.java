package org.enso.common;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.ServiceLoader;

/**
 * Generic support for loading Java polyglot symbols. The resolver provides two kinds of interfaces:
 *
 * <ul>
 *   <li>the client API - represented by all the <b>public static</b> methods
 *   <li>the SPI - e.g. service provider interface - those are the <b>protected abstract</b> methods
 * </ul>
 *
 * Those who tend to extend the capabilities of loading Java classes into Enso runtime shall
 * register their own implementation visible via {@link ServiceLoader}.
 *
 * @see RuntimeOptions#HOST_CLASS_LOADING
 */
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
    ClassNotFoundException ex = null;
    for (var p : ALL) {
      try {
        var found = p.handleLoadClass(name);
        assert found != null;
        return found;
      } catch (ClassNotFoundException cnfe) {
        ex = cnfe;
      }
    }
    if (ex == null) {
      throw new ClassNotFoundException(name);
    } else {
      throw ex;
    }
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
