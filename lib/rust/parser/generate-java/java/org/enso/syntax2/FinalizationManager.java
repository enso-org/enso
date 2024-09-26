package org.enso.syntax2;

import java.lang.ref.*;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Set;

final class FinalizationManager {
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
  <T> void attachFinalizer(T referent, Runnable finalize) {
    finalizers.add(new FinalizationReference(referent, finalize, referenceQueue));
  }

  void runPendingFinalizers() {
    var ref = referenceQueue.poll();
    while (ref != null) {
      runFinalizer(ref);
      ref = referenceQueue.poll();
    }
  }

  /**
   * @return The finalizers that have been registered, and have not yet been run.
   *     <p>This does not de-register the finalizers; they will still be run as usual after their
   *     reference objects become unreachable.
   */
  Iterable<Runnable> getRegisteredFinalizers() {
    synchronized (finalizers) {
      return finalizers.stream().map(ref -> ref.finalize).toList();
    }
  }

  private void runFinalizer(Reference<?> ref) {
    if (ref instanceof FinalizationReference) {
      var finalizationReference = (FinalizationReference) ref;
      finalizationReference.finalize.run();
      finalizers.remove(finalizationReference);
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
