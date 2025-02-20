package org.enso.base.spi;

import java.util.List;
import java.util.ServiceLoader;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import org.graalvm.polyglot.Value;

/** A helper class that allows loading Enso services from the classpath. */
public final class EnsoServiceLoader<T extends EnsoService> {
  private final ServiceLoader<T> loader;
  private transient List<T> cachedProviders = null;

  private EnsoServiceLoader(Class<T> clazz) {
    loader = ServiceLoader.load(clazz, clazz.getClassLoader());
  }

  public static <T extends EnsoService> EnsoServiceLoader<T> load(Class<T> clazz) {
    return new EnsoServiceLoader<>(clazz);
  }

  /** Reloads the services in case the classpath has changed. */
  public final void reload() {
    cachedProviders = null;
    loader.reload();
  }

  public final List<T> getProviders() {
    if (cachedProviders == null) {
      cachedProviders =
          loader.stream().map(ServiceLoader.Provider::get).filter(EnsoService::isLoaded).toList();
    }

    return cachedProviders;
  }

  public final List<Value> getTypeObjects() {
    return getProviders().stream().map(EnsoService::getTypeObject).toList();
  }

  /**
   * Finds a single provider among available ones that matches the given predicate.
   *
   * <p>The predicate should uniquely identify a single service, thus if it matches more than one
   * entry it is considered an error - it should only happen if two libraries mistakenly provide a
   * clashing service name and cannot be distinguished.
   */
  public final T findSingleProvider(Predicate<T> predicate, String predicateDescription) {
    var found = getProviders().stream().filter(predicate).toList();
    if (found.isEmpty()) {
      return null;
    } else if (found.size() > 1) {
      var modules =
          found.stream().map(EnsoService::getModuleName).collect(Collectors.joining(", "));
      throw new IllegalStateException(
          "Multiple providers found for "
              + predicateDescription
              + ". The clashing definitions are in the following modules: "
              + modules
              + ".");
    } else {
      return found.get(0);
    }
  }
}
