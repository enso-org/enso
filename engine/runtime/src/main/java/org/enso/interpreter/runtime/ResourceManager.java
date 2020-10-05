package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Resource;
import org.enso.syntax.text.Phantom;

import java.lang.ref.Cleaner;
import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class ResourceManager {
  private final Context context;
  private volatile boolean isClosed = false;
  private volatile Thread workerThread;
  private final Runner worker = new Runner();
  private final ReferenceQueue<Resource> referenceQueue = new ReferenceQueue<>();
  private final ConcurrentMap<PhantomReference<Resource>, Item> items = new ConcurrentHashMap<>();

  public ResourceManager(Context context) {
    this.context = context;
  }

  @CompilerDirectives.TruffleBoundary
  public void park(Resource resource) {
    Item it = items.get(resource.getPhantomReference());
    if (it == null) {
      throw new IllegalStateException("A resource is being parked but it was already finalized.");
    }
    it.getParkedCount().incrementAndGet();
  }

  @CompilerDirectives.TruffleBoundary
  public void unpark(Resource resource) {
    Item it = items.get(resource.getPhantomReference());
    if (it == null) {
      throw new IllegalStateException(
          "A resource is being unparked, but it was already finalized.");
    }
    it.getParkedCount().decrementAndGet();
    tryFinalize(it);
  }

  private void tryFinalize(Item it) {
    if (it.isFlaggedForFinalization().get()) {
      // We already know that isFlaggedForFinalization was true at some
      // point and there are no other threads still parking the underlying
      // value. Note that it is impossible for parked count to increase after
      // the value is flagged for finalization (as parking the value requires
      // a live reference). Attempt finalization.
      if (it.getParkedCount().get() == 0) {
        // Check if another thread didn't get to this branch first.
        boolean continueFinalizing = it.isFlaggedForFinalization().compareAndSet(true, false);
        if (continueFinalizing) {
          it.doFinalize(context);
          items.remove(it.reference);
        }
      }
    }
  }

  public Resource register(Object object, Object function) {
    if (isClosed) {
      throw new IllegalStateException("Can't register new resources after context is closed.");
    }
    if (workerThread == null || !workerThread.isAlive()) {
      worker.setKilled(false);
      workerThread = context.createThread(worker);
      workerThread.start();
    }
    Resource resource = new Resource(object);
    PhantomReference<Resource> ref = new PhantomReference<>(resource, referenceQueue);
    resource.setPhantomReference(ref);
    items.put(ref, new Item(object, function, ref));
    return resource;
  }

  public void close() {
    isClosed = true;
    worker.setKilled(true);
    if (workerThread != null) {
      while (true) {
        try {
          workerThread.interrupt();
          workerThread.join();
          break;
        } catch (InterruptedException ignored) {
        }
      }
    }
    for (PhantomReference<Resource> key : items.keySet()) {
      Item it = items.remove(key);
      if (it != null) {
        // Remove unconditionally – all other threads are supposed to be dead
        // by now.
        it.doFinalize(context);
      }
    }
  }

  private class Runner implements Runnable {
    private volatile boolean killed = false;

    @Override
    public void run() {
      while (true) {
        try {
          Reference<? extends Resource> ref = referenceQueue.remove();
          if (!killed) {
            Item it = items.get(ref);
            it.isFlaggedForFinalization().set(true);
            tryFinalize(it);
          }
          if (killed) {
            return;
          }
        } catch (InterruptedException e) {
          if (killed) {
            return;
          }
        }
      }
    }

    public void setKilled(boolean killed) {
      this.killed = killed;
    }
  }

  private static class Item {
    private final Object underlying;
    private final Object finalizer;
    private final PhantomReference<Resource> reference;
    private final AtomicInteger parkedCount = new AtomicInteger();
    private final AtomicBoolean flaggedForFinalization = new AtomicBoolean();

    public Item(Object underlying, Object finalizer, PhantomReference<Resource> reference) {
      this.underlying = underlying;
      this.finalizer = finalizer;
      this.reference = reference;
    }

    public void doFinalize(Context context) {
      context.getThreadManager().enter();
      try {
        InteropLibrary.getUncached(finalizer).execute(finalizer, underlying);
      } catch (Exception ignored) {
        context.getErr().println("Exception in finalizer: " + ignored.getMessage());
      } finally {
        context.getThreadManager().leave();
      }
    }

    public AtomicInteger getParkedCount() {
      return parkedCount;
    }

    public AtomicBoolean isFlaggedForFinalization() {
      return flaggedForFinalization;
    }
  }
}
