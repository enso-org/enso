package org.enso.base.lookup;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;
import java.util.stream.Stream;

public final class Lookup<T> implements Iterable<T> {
  private final Class<T> service;
  private List<ServiceLoader.Provider<T>> found;

  private Lookup(Class<T> service) {
    this.service = service;
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
    var arr = new ArrayList<ServiceLoader.Provider<T>>();
    if (System.getProperties().get("enso.class.path") instanceof Collection<?> layers) {
      for (var obj : layers) {
        if (obj instanceof ModuleLayer layer) {
          ServiceLoader.load(layer, service).stream()
              .forEach(
                  (p) -> {
                    arr.add(p);
                  });
        }
      }
    }
    return arr;
  }

  public static <S> Lookup<S> lookup(Class<S> service) {
    return new Lookup<>(service);
  }
}
