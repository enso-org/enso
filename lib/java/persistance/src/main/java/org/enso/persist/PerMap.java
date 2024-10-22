package org.enso.persist;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import org.openide.util.lookup.Lookups;

final class PerMap {

  private static final int serialVersionUID = 8652; // Use PR number
  private static final Collection<? extends Persistance> ALL;

  static {
    var loader = PerMap.class.getClassLoader();
    var lookup = Lookups.metaInfServices(loader);
    var all = new ArrayList<Persistance>();
    all.add(PerReferencePeristance.INSTANCE);
    all.addAll(lookup.lookupAll(Persistance.class));
    ALL = all;
  }

  private final Map<Integer, Persistance<?>> ids = new HashMap<>();
  private final Map<Class<?>, Persistance<?>> types = new HashMap<>();
  final int versionStamp;

  private PerMap() {
    int hash = serialVersionUID;
    for (var orig : ALL) {
      var p = orig.newClone();
      var prevId = ids.put(p.id, p);
      if (prevId != null) {
        throw new IllegalStateException(
            "Multiple registrations for ID " + p.id + " " + prevId + " != " + p);
      }
      hash = Objects.hash(hash, p.id);
      var prevType = types.put(p.clazz, p);
      if (prevType != null) {
        throw new IllegalStateException(
            "Multiple registrations for " + p.clazz.getName() + " " + prevId + " != " + p);
      }
    }
    versionStamp = hash;
  }

  static PerMap create() {
    return new PerMap();
  }

  @SuppressWarnings(value = "unchecked")
  private synchronized <T> Persistance<T> searchSupertype(Class<T> type) {
    // synchronized as it mutes the types map
    // however over time the types map gets saturated and
    // the synchronization will get less frequent
    // please note that Persistance as well as Class (as a key) have all fields final =>
    // as soon as they become visible from other threads, they have to look consistent
    if (type == null) {
      return null;
    }

    var direct = types.get(type);
    if (direct != null) {
      if (!direct.includingSubclasses) {
        return null;
      }

      return (Persistance<T>) direct;
    }

    for (java.lang.Class<?> in : type.getInterfaces()) {
      var forSuperInterfaces = searchSupertype(in);
      if (forSuperInterfaces != null) {
        types.put(type, forSuperInterfaces);
        return (Persistance<T>) forSuperInterfaces;
      }
    }

    if (!type.isInterface()) {
      var forSuperclass = searchSupertype(type.getSuperclass());
      if (forSuperclass != null) {
        types.put(type, forSuperclass);
        return (Persistance<T>) forSuperclass;
      }
    }

    return null;
  }

  @SuppressWarnings(value = "unchecked")
  final <T> Persistance<T> forType(Class<T> type) {
    org.enso.persist.Persistance<?> p = types.get(type);
    if (p == null) {
      p = searchSupertype(type);
    }
    if (p == null) {
      throw PerUtils.raise(
          RuntimeException.class, new IOException("No persistance for " + type.getName()));
    }
    return (Persistance<T>) p;
  }

  final Persistance<?> forId(int id) {
    org.enso.persist.Persistance<?> p = ids.get(id);
    if (p == null) {
      throw PerUtils.raise(RuntimeException.class, new IOException("No persistance for " + id));
    }
    return p;
  }
}
