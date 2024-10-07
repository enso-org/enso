package org.enso.base.lookup;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ServiceLoader;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;

public final class Lookup<T> implements Iterable<T> {
  private final Function<ModuleLayer, ServiceLoader<T>> factory;
  private List<ServiceLoader.Provider<T>> found;
  private static final Logger logger = Logger.getLogger(Lookup.class.getName());

  private Lookup(Function<ModuleLayer, ServiceLoader<T>> factory) {
    this.factory = factory;
  }

  @Override
  public Iterator<T> iterator() {
    return stream().map(p -> p.get()).toList().iterator();
  }

  public Stream<ServiceLoader.Provider<T>> stream() {
    var tmp = found;
    if (tmp == null) {
      found = tmp = findAll();
    }
    return tmp.stream();
  }

  public void reload() {
    found = null;
  }

  private List<ServiceLoader.Provider<T>> findAll() {
    var serviceProviders = new LinkedHashMap<ServiceLoader.Provider<T>, ModuleLayer>();
    if (System.getProperties().get("enso.class.path") instanceof Collection<?> layers) {
      for (var obj : layers) {
        if (obj instanceof ModuleLayer layer) {
          factory.apply(layer).stream()
              .forEach(
                  (p) -> {
                    if (serviceProviders.containsKey(p)) {
                      var prevLayer = serviceProviders.get(p);
                      logger.log(
                          Level.WARNING,
                          String.format(
                              "Duplicate provider found: %s. Previous provider in layer '%s'."
                                  + " Current provider in layer '%s'.",
                              providerToString(p), prevLayer, layer));
                    }
                    serviceProviders.put(p, layer);
                  });
        }
      }
    }
    return serviceProviders.keySet().stream().toList();
  }

  public static <S> Lookup<S> lookup(Function<ModuleLayer, ServiceLoader<S>> factory) {
    return new Lookup<>(factory);
  }

  private static <T> String providerToString(ServiceLoader.Provider<T> provider) {
    var mod = provider.type().getModule();
    var modLayer = mod.getLayer();
    var tp = provider.type();
    return "Provider[type='"
        + tp
        + "', module='"
        + mod.getName()
        + "', moduleLayer={"
        + modLayer
        + "}]";
  }
}
