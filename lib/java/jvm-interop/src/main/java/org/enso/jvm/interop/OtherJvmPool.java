package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.HashMap;
import java.util.Map;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistance;

/** Pool of Truffle objects associated with {@link Channel}. */
public final class OtherJvmPool extends Channel.Config {
  private static final Map<Long, TruffleObject> OBJECTS = new HashMap<>();

  synchronized long registerObject(TruffleObject obj) {
    var size = OBJECTS.size() + 1;
    OBJECTS.put((long) size, obj);
    return size;
  }

  synchronized TruffleObject findObject(long id) {
    return OBJECTS.get(id);
  }

  @Override
  @SuppressWarnings("unchecked")
  public final Persistance.Pool createPool(Channel<?> channel) {
    return Persistance.Pool.withReplaceRewrite(
        Persistables.POOL,
        (obj) ->
            switch (obj) {
              case OtherJvmObject other -> {
                if (other.id() < 0) {
                  // the other object with negative number came back
                  // it is our own object
                  var ourOwn = findObject(-other.id());
                  assert ourOwn != null;
                  yield ourOwn;
                } else {
                  var proxy = OtherJvmObject.bindToChannel(other, (Channel<OtherJvmPool>) channel);
                  yield proxy;
                }
              }
              default -> obj;
            },
        (obj) ->
            switch (obj) {
              case OtherJvmObject other -> {
                assert other.id() < 0
                    : "Returning back an OtherJvmObject. It is from the other JVM - e.g. it has"
                          + " negative number";
                yield other;
              }
              case TruffleObject foreign -> {
                var id = registerObject(foreign);
                // our own objects send to the other side should
                // have negative ID
                yield new OtherJvmObject(null, -id);
              }
              default -> obj;
            });
  }
}
