package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.HashMap;
import java.util.Map;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistance;

/** Pool of Truffle objects associated with {@link Channel}. */
public final class OtherJvmPool extends Channel.Config {
  private static final Map<Long, TruffleObject> OBJECTS = new HashMap<>();

  static synchronized long registerObject(TruffleObject obj) {
    var size = OBJECTS.size() + 1;
    OBJECTS.put((long) size, obj);
    return size;
  }

  static synchronized TruffleObject findObject(long id) {
    return OBJECTS.get(id);
  }

  @Override
  public final Persistance.Pool pool() {
    return Persistables.POOL;
  }
}
