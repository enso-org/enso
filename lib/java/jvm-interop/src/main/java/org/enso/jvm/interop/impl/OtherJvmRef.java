package org.enso.jvm.interop.impl;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;
import org.enso.jvm.channel.Channel;

final class OtherJvmRef extends WeakReference<OtherJvmObject> {

  private static final ReferenceQueue<? super OtherJvmObject> ALIVE = new ReferenceQueue<>();
  private static final List<OtherJvmRef> KEEP = new ArrayList<>();
  private final long id;
  private final Channel<OtherJvmPool> channel;

  private OtherJvmRef(OtherJvmObject referent, Channel<OtherJvmPool> ch) {
    super(referent, ALIVE);
    this.id = referent.id();
    this.channel = ch;
    assert this.channel != null;
  }

  @Override
  public String toString() {
    return "Ref{" + "id=" + id + '}';
  }

  static synchronized void registerGCable(OtherJvmObject other, Channel<OtherJvmPool> ch) {
    KEEP.add(new OtherJvmRef(other, ch));
  }

  static synchronized void closeChannel(Channel<OtherJvmPool> ch) {
    var it = KEEP.iterator();
    while (it.hasNext()) {
      var ref = it.next();
      if (ref.channel == ch) {
        ref.enqueue();
        it.remove();
      }
    }
    flushQueue(ch);
  }

  static void flushQueue(Channel<OtherJvmPool> beingClosed) {
    while (true) {
      org.enso.jvm.interop.impl.OtherJvmRef r = (OtherJvmRef) ALIVE.poll();
      if (r == null) {
        break;
      }
      if (r.channel == beingClosed) {
        // no need to deliver messages
        continue;
      }
      r.channel.execute(Void.class, new OtherJvmMessage.GC(r.id));
      synchronized (OtherJvmRef.class) {
        KEEP.remove(r);
      }
    }
  }
}
