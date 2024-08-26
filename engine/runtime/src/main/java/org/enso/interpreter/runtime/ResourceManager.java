package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.interop.InteropLibrary;
import java.lang.ref.PhantomReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import org.enso.interpreter.runtime.data.ManagedResource;

/** Allows the context to attach garbage collection hooks on the removal of certain objects. */
public final class ResourceManager {
  /** how many milliseconds to wait for another resource when no is available */
  private static final long KEEP_ALIVE = 1000;

  /** Queue with resources eligible for finalization */
  private final ReferenceQueue<ManagedResource> referenceQueue = new ReferenceQueue<>();

  private final EnsoContext context;

  /**
   * @GuardedBy("this")
   */
  private ProcessItems processor;

  /**
   * @GuardedBy("this")
   */
  private boolean isClosed;

  /**
   * Creates a new instance of Resource Manager.
   *
   * @param context the language context owning the new instance
   */
  public ResourceManager(EnsoContext context) {
    this.context = context;
  }

  /**
   * Puts the finalization of the resource on hold, until {@link #unpark(ManagedResource)} is
   * called. The resource won't be finalized, even if it becomes unreachable between the calls.
   *
   * @param resource the resource to park.
   */
  public void park(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      it.park();
    }
  }

  /**
   * Resumes finalization of the resource. If the resource has become unreachable and there are no
   * other threads parking the resource, it will get finalized right away.
   *
   * @param resource the resource to unpark.
   */
  public void unpark(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      if (it.unpark(context)) {
        removeFromItems(it);
      }
    }
  }

  /**
   * Manually and unconditionally finalizes the resource. Ignores the parking mechanism, assuming
   * the user now has full control over the resource.
   *
   * @param resource the resource to finalize.
   */
  @CompilerDirectives.TruffleBoundary
  public void close(ManagedResource resource) {
    if (resource.getPhantomReference() instanceof Item it) {
      removeFromItems(it);
      // Unconditional finalization – user controls the resource manually.
      it.finalizeNow(context);
    }
  }

  /**
   * Removes the resource from the system, cancelling any automatic finalization action attached to
   * it.
   *
   * @param resource the resource to take away from this system.
   */
  @CompilerDirectives.TruffleBoundary
  public void take(ManagedResource resource) {
    removeFromItems(resource.getPhantomReference());
  }

  /**
   * Registers a new resource to the system. {@code function} will be called on {@code object} when
   * the value returned by this method becomes unreachable.
   *
   * @param object the underlying resource
   * @param function the finalizer action to call on the underlying resource
   * @return a wrapper object, containing the resource and serving as a reachability probe
   */
  @CompilerDirectives.TruffleBoundary
  public synchronized ManagedResource register(Object object, Object function) {
    if (isClosed) {
      throw EnsoContext.get(null)
          .raiseAssertionPanic(
              null, "Can't register new resources after resource manager is closed.", null);
    }
    var resource = new ManagedResource(object, r -> new Item(r, object, function, referenceQueue));
    var ref = (Item) resource.getPhantomReference();
    addPendingItem(ref);
    return resource;
  }

  /**
   * Stops this system, stops and joins any threads created by it. Unconditionally finalizes all the
   * registered resources, ignoring their reachability status.
   *
   * <p>This is only useful when the underlying context is being finalized and no more user code
   * will be run in it.
   */
  public void shutdown() {
    Collection<Item> toFinalize;
    synchronized (this) {
      isClosed = true;
      if (processor != null) {
        toFinalize = processor.shutdown();
      } else {
        toFinalize = Collections.emptyList();
      }
    }
    for (var it : toFinalize) {
      // Finalize unconditionally – all other threads are dead by now.
      it.finalizeNow(context);
    }
  }

  /**
   * Adds pending item into the existing processor. If there is no processor, it allocates new and
   * starts its processing thread.
   */
  @CompilerDirectives.TruffleBoundary
  private synchronized void addPendingItem(Item item) {
    if (processor == null) {
      processor = new ProcessItems(r -> context.createThread(true, r));
    }
    processor.add(item);
  }

  @CompilerDirectives.TruffleBoundary
  private synchronized void removeFromItems(PhantomReference<ManagedResource> it) {
    if (processor != null && it instanceof Item item) {
      processor.remove(item);
    }
  }

  @CompilerDirectives.TruffleBoundary
  private synchronized void shutdownProcessorIfNoPending() {
    if (processor != null && processor.isPendingEmpty()) {
      processor.killed = true;
      processor = null;
    }
  }

  /**
   * Processes {@link Item}s eligible for GC. Plays two roles. First of all cleans {@link
   * #referenceQueue} in {@link #run()} method running in its own thread. Then it invokes finalizers
   * in {@link #perform} method inside of Enso execution context.
   */
  private final class ProcessItems extends ThreadLocalAction implements Runnable {
    /**
     * @GuardedBy("ResourceManager.this")
     */
    private final List<Item> pendingItems = new ArrayList<>();

    /**
     * @GuardedBy("toFinalize")
     */
    private final List<Item> toFinalize = new ArrayList<>();

    /**
     * @GuardedBy("toFinalize")
     */
    private Future<Void> safepointRequest;

    private final Thread workerThread;

    private volatile boolean killed;

    ProcessItems(Function<Runnable, Thread> threadFactory) {
      super(false, false, true);
      this.workerThread = threadFactory.apply(this);
      this.workerThread.start();
    }

    /** Registers another Item for this processor. */
    final void add(Item item) {
      assert Thread.holdsLock(ResourceManager.this);
      pendingItems.add(item);
    }

    /** Removes an Item from this processor. */
    final void remove(Item item) {
      assert Thread.holdsLock(ResourceManager.this);
      pendingItems.remove(item);
      if (pendingItems.isEmpty()) {
        workerThread.interrupt();
      }
    }

    /** Is pending list empty? */
    final boolean isPendingEmpty() {
      synchronized (ResourceManager.this) {
        return pendingItems.isEmpty();
      }
    }

    /**
     * Runs at a safe point in middle of regular Enso program execution. Gathers all available
     * {@link #toFinalize} and runs their finalizers. Removes all processed items from {@link
     * #toFinalize}. If there are any remaining, continues processing them. Otherwise finishes.
     *
     * @param access not used for anything
     */
    @Override
    protected void perform(ThreadLocalAction.Access access) {
      var isMyThreadChoosen = false;
      for (; ; ) {
        Item[] toProcess;
        synchronized (toFinalize) {
          if (!isMyThreadChoosen) {
            if (safepointRequest == null || safepointRequest.isCancelled()) {
              // some thread is already handing the safepointRequest
              return;
            } else {
              // I am choosen and I will loop and process toFinalize
              // until they are available
              isMyThreadChoosen = true;
              // signal others this safepointRequest has choosen thread
              safepointRequest.cancel(false);
            }
          }
          if (toFinalize.isEmpty()) {
            // nothing to process anymore,
            // signal safepointRequest is finished and new one shall be scheduled
            safepointRequest = null;
            return;
          }
          toProcess = toFinalize.toArray(Item[]::new);
          toFinalize.clear();
        }
        for (var it : toProcess) {
          it.finalizeNow(context);
          removeFromItems(it);
        }
      }
    }

    /**
     * Running in its own thread. Waiting for {@link #referenceQueue} to be populated with GCed
     * items. Scheduling {@link #perform} action at safe points while passing the {@link Item}s to
     * it via {@link #toFinalize}.
     */
    @Override
    public void run() {
      while (true) {
        try {
          Reference<? extends ManagedResource> ref;
          if (isPendingEmpty()) {
            ref = referenceQueue.remove(KEEP_ALIVE);
            if (ref == null) {
              shutdownProcessorIfNoPending();
            }
          } else {
            ref = referenceQueue.remove();
          }
          if (!killed) {
            if (ref instanceof Item it) {
              it.flaggedForFinalization.set(true);
              synchronized (toFinalize) {
                if (safepointRequest == null) {
                  safepointRequest = context.submitThreadLocal(null, this);
                }
                toFinalize.add(it);
              }
              synchronized (ResourceManager.this) {
                remove(it);
              }
            }
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

    /**
     * Sets the killed flag of this thread. This flag being set to {@code true} will force it to
     * stop execution at the soonest possible safe point. Other than setting this flag, the thread
     * should also be interrupted to read it, in case it is blocked on an interruptible operation.
     *
     * @return the list of items that deserve to be finalized
     */
    Collection<Item> shutdown() {
      this.killed = true;
      workerThread.interrupt();
      while (workerThread.isAlive()) {
        try {
          workerThread.join();
        } catch (InterruptedException ex) {
        }
      }
      var all = new HashSet<Item>();
      all.addAll(toFinalize);
      all.addAll(pendingItems);
      return all;
    }
  }

  /** A storage representation of a finalizable object handled by this system. */
  private static final class Item extends PhantomReference<ManagedResource> {
    private final Object underlying;
    private final Object finalizer;

    /**
     * Returns the counter of actions parking this object. The object can be safely finalized only
     * if it's unreachable {@link #isFlaggedForFinalization()} and this counter is zero.
     *
     * @return the parking actions counter
     */
    private final AtomicInteger parkedCount = new AtomicInteger();

    /**
     * Returns the boolean representing finalization status of this object. The object should be
     * removed by the first thread that observes this flag to be set to true and the {@link
     * #getParkedCount()} to be zero. If a thread intends to perform the finalization, it should set
     * this flag to {@code false}.
     *
     * @return the finalization flag
     */
    private final AtomicBoolean flaggedForFinalization = new AtomicBoolean();

    /**
     * Creates a new finalizable item.
     *
     * @param underlying the underlying object that should be finalized
     * @param finalizer the finalizer to run on the underlying object
     * @param reference a phantom reference used for tracking the reachability status of the
     *     resource.
     */
    private Item(
        ManagedResource referent,
        Object underlying,
        Object finalizer,
        ReferenceQueue<ManagedResource> queue) {
      super(referent, queue);
      this.underlying = underlying;
      this.finalizer = finalizer;
    }

    /**
     * Performs the finalization action of this resource right now. The thread must be inside of a
     * context.
     *
     * @param context current execution context
     */
    @CompilerDirectives.TruffleBoundary
    private void finalizeNow(EnsoContext context) {
      try {
        InteropLibrary.getUncached(finalizer).execute(finalizer, underlying);
      } catch (Exception e) {
        context.getErr().println("Exception in finalizer: " + e.getMessage());
      }
    }

    private void park() {
      parkedCount.incrementAndGet();
    }

    /**
     * @return {@code true} if the finalizer was run
     */
    private boolean unpark(EnsoContext context) {
      if (parkedCount.decrementAndGet() == 0) {
        boolean continueFinalizing = flaggedForFinalization.compareAndSet(true, false);
        if (continueFinalizing) {
          finalizeNow(context);
          return true;
        }
      }
      return false;
    }
  }
}
