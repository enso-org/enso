package org.enso.jvm.interop;

import com.oracle.truffle.api.interop.TruffleObject;
import java.util.HashMap;
import java.util.Map;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistance;

/** Pool of Truffle objects associated with {@link Channel}. */
public final class OtherJvmPool extends Channel.Config {
  private final Map<Long, TruffleObject> objectsById = new HashMap<>();

  /** context to use when entering tests */
  final TruffleClassLoader loader = new TruffleClassLoader();

  private synchronized long registerObject(TruffleObject obj) {
    var size = objectsById.size() + 1;
    objectsById.put((long) size, obj);
    return size;
  }

  final synchronized TruffleObject findObject(long id) {
    return objectsById.get(id);
  }

  @Override
  @SuppressWarnings("unchecked")
  public final Persistance.Pool createPool(Channel<?> channel) {
    var withRead =
        Persistables.POOL.withReadResolve(
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
                      // real truffle object in the other JVM
                      // need to keep it as OtherJvmObject proxy
                      // just associate channel to it
                      var proxy =
                          OtherJvmObject.bindToChannel(other, (Channel<OtherJvmPool>) channel);
                      yield proxy;
                    }
                  }
                  case null -> null;
                  default -> obj;
                });
    var withReadAndWrite =
        withRead.withWriteReplace(
            (obj) ->
                switch (obj) {
                  case OtherJvmObject other -> {
                    // returning back their own OtherJvmObject - let
                    // them know it is theirs by using negative ID
                    yield new OtherJvmObject(null, -other.id());
                  }
                  case TruffleObject foreign -> {
                    var id = registerObject(foreign);
                    // our own truffle objects send to the other side should
                    // have a positive ID
                    yield new OtherJvmObject(null, id);
                  }
                  case null -> null;
                  default -> obj;
                });
    return withReadAndWrite;
  }
}
