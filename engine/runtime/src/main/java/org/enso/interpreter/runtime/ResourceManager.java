package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Resource;

import java.lang.ref.Cleaner;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class ResourceManager {
  private final Cleaner cleaner;
  private final Context context;
  private volatile boolean isClosed = false;

  private final ThreadLocal<Set<Resource>> parkedResources = new ThreadLocal<>();
  private final ConcurrentMap<Object, Cleaner.Cleanable> cleanables = new ConcurrentHashMap<>();

  public ResourceManager(Context context) {
    this.context = context;
    this.cleaner = Cleaner.create(this.context::createThread);
  }

  @CompilerDirectives.TruffleBoundary
  public void park(Resource resource) {
    Set<Resource> resources = parkedResources.get();
    if (resources == null) {
      resources = new HashSet<>();
      parkedResources.set(resources);
    }
    resources.add(resource);
  }

  @CompilerDirectives.TruffleBoundary
  public void unpark(Resource resource) {
    Set<Resource> resources = parkedResources.get();
    if (!(resources == null)) {
      resources.remove(resource);
    }
  }

  public Resource register(Object object, Function function) {
    if (isClosed) {
      throw new IllegalStateException("Can't register new resources after context is closed.");
    }
    Resource resource = new Resource(object);
    Object id = new Object();
    Cleaner.Cleanable cleanable =
        cleaner.register(resource, new CleanRunner(context, cleanables, id, object, function));
    cleanables.put(id, cleanable);
    return resource;
  }

  public void close() {
    isClosed = true;
    for (Object key : cleanables.keySet()) {
      Cleaner.Cleanable cleanable = cleanables.remove(key);
      if (cleanable != null) {
        cleanable.clean();
      }
    }
  }

  private static class CleanRunner implements Runnable {
    private final Context context;
    private final ConcurrentMap<Object, Cleaner.Cleanable> cleanables;
    private final Object id;
    private final Object underlying;
    private final Function function;

    public CleanRunner(
        Context context,
        ConcurrentMap<Object, Cleaner.Cleanable> cleanables,
        Object id,
        Object underlying,
        Function function) {
      this.context = context;
      this.cleanables = cleanables;
      this.id = id;
      this.underlying = underlying;
      this.function = function;
    }

    @Override
    public void run() {
      cleanables.remove(id);
      context.getThreadManager().enter();
      try {
        InteropLibrary.getUncached(function).execute(function, underlying);
      } catch (UnsupportedTypeException | UnsupportedMessageException | ArityException ignored) {
      } finally {
        context.getThreadManager().leave();
      }
    }
  }
}
