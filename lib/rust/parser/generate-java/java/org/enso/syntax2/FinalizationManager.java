package org.enso.syntax2;

import java.lang.ref.*;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;

public final class FinalizationManager {
  private final ReferenceQueue<Object> referenceQueue = new ReferenceQueue<>();
  private final Set<FinalizationReference> finalizers =
      Collections.synchronizedSet(Collections.newSetFromMap(new IdentityHashMap<>()));

  /**
   * Associate the given callback with an object so that if the object is freed, the callback will
   * be scheduled to be run. Note that the callback is not guaranteed to be executed, e.g. if the
   * process exits first.
   *
   * @param referent Object whose lifetime determines when the finalizer will be run.
   * @param finalize Callback to run after {@code referent} has been garbage-collected.
   */
  public <T> void attachFinalizer(T referent, Runnable finalize) {
    finalizers.add(new FinalizationReference(referent, finalize, referenceQueue));
  }

  public FinalizerRunner createRunner() {
    return this.new FinalizerRunner();
  }

  public class FinalizerRunner implements Runnable {
    @Override
    public void run() {
      while (true) {
        try {
          var ref = referenceQueue.remove();
          if (ref instanceof FinalizationReference finalizationReference) {
            finalizationReference.finalize.run();
            finalizers.remove(finalizationReference);
          }
        } catch (InterruptedException ignored) {
        }
      }
    }
  }

  private static class FinalizationReference extends PhantomReference<Object> {
    final Runnable finalize;

    FinalizationReference(Object referent, Runnable finalize, ReferenceQueue<? super Object> q) {
      super(referent, q);
      this.finalize = finalize;
    }
  }
}
