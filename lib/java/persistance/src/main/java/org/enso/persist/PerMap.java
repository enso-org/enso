package org.enso.persist;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import org.enso.persist.Persistance.Reference;
import org.openide.util.lookup.Lookups;

final class PerMap {

  private static final int serialVersionUID = 8689; // Use PR number
  private static final Collection<? extends Persistance> ALL;

  static {
    var loader = PerMap.class.getClassLoader();
    var lookup = Lookups.metaInfServices(loader);
    var all = new ArrayList<Persistance>();
    all.add(ReferencePersitance.INSTANCE);
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
      hash += p.id;
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
  private synchronized <T> Persistance<T> searchSupertype(String name, Class<T> type) {
    // synchronized as it mutes the types map
    // however over time the types map gets saturated and
    // the synchronization will get less frequent
    // please note that Persistance as well as Class (as a key) have all fields final =>
    // as soon as they become visible from other threads, they have to look consistent
    NOT_FOUND:
    if (type != null) {
      org.enso.persist.Persistance<?> p = types.get(type);
      if (p != null) {
        if (!p.includingSubclasses) {
          break NOT_FOUND;
        }
      } else {
        for (java.lang.Class<?> in : type.getInterfaces()) {
          p = searchSupertype(name, in);
          if (p != null) {
            break;
          }
        }
        if (p == null && !type.isInterface()) {
          p = searchSupertype(name, type.getSuperclass());
        }
        types.put(type, p);
      }
      return (Persistance<T>) p;
    }
    if (type == null) {
      throw PerUtils.raise(RuntimeException.class, new IOException("No persistance for " + name));
    } else {
      return null;
    }
  }

  @SuppressWarnings(value = "unchecked")
  final <T> Persistance<T> forType(Class<T> type) {
    org.enso.persist.Persistance<?> p = types.get(type);
    if (p == null) {
      p = searchSupertype(type.getName(), type);
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

  private static class ReferencePersitance extends Persistance<Persistance.Reference> {
    private static final ReferencePersitance INSTANCE = new ReferencePersitance();

    private ReferencePersitance() {
      super(Reference.class, true, 60941);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(Reference obj, Output out) throws IOException {
      out.writeObject(obj.get(Object.class));
    }

    @Override
    protected Reference readObject(Input in) throws IOException, ClassNotFoundException {
      return in.readReference(Object.class);
    }
  }
}
