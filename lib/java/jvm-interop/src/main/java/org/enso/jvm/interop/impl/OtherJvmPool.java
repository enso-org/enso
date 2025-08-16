package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.nodes.Node;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Function;
import org.enso.jvm.channel.Channel;
import org.enso.persist.Persistance;

/** Pool of Truffle objects associated with {@link Channel}. */
public final class OtherJvmPool extends Channel.Config {
  private final Map<Long, TruffleObject> objectsById = new HashMap<>();

  /** context to use when entering tests */
  private OtherJvmLoader loader;

  private Function<Node, Object> onEnter;
  private BiConsumer<Node, Object> onLeave;

  /** Master Channel can be associated with actions on enter and on leave. */
  public final void onEnterLeave(Function<Node, Object> onEnter, BiConsumer<Node, Object> onLeave) {
    this.onEnter = onEnter;
    this.onLeave = onLeave;
  }

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
                  case OtherJvmTruffleException ex -> {
                    // unwrap the exception to object reference
                    // and send it back as regular OtherJvmObject
                    yield new OtherJvmObject(null, -ex.delegate.id());
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

  Object enter(boolean master, Node node) {
    if (master) {
      if (onEnter != null) {
        return onEnter.apply(node);
      }
    } else {
      loader(master).ctx.enter();
    }
    return null;
  }

  void leave(boolean master, Node node, Object prev) {
    if (master) {
      if (onLeave != null) {
        onLeave.accept(node, prev);
      }
    } else {
      loader(master).ctx.leave();
    }
  }

  void addToClassPath(boolean master, String file) {
    loader(master).addToClassPath(file);
  }

  final TruffleObject loadClassObject(boolean master, String className)
      throws ClassNotFoundException {
    var clazz = loader(master).loadClassObject(className);
    return clazz;
  }

  private final synchronized OtherJvmLoader loader(boolean master) {
    assert !master : "Cannot handle classloading in master, only in slave";
    if (loader == null) {
      loader = new OtherJvmLoader();
    }
    return loader;
  }

  //
  // Support for histogram of messages
  //

  /**
   * Enable histogram of messages for example by:
   *
   * <pre>
   * runEngineDistribution
   *    --vm.D=org.enso.jvm.interop.limit=100000
   *    --vm.D=polyglot.enso.classLoading=guest
   *    --run test/Generic_JDBC_Tests
   * </pre>
   */
  private static final int DUMP_MESSAGES_COUNT =
      Integer.getInteger("org.enso.jvm.interop.limit", -1);

  /**
   * @GuardedBy("this")
   */
  private Map<Message, AtomicInteger> histogram;

  /**
   * @GuardedBy("this")
   */
  private int countDown;

  private synchronized void incrementMessage(Message message) {
    assert DUMP_MESSAGES_COUNT > 0;
    if (histogram == null) {
      histogram = new ConcurrentHashMap<>();
      countDown = DUMP_MESSAGES_COUNT;
    }
    var count = histogram.computeIfAbsent(message, (ignore) -> new AtomicInteger());
    count.incrementAndGet();
    if (countDown-- < 0) {
      dumpMessages();
      countDown = DUMP_MESSAGES_COUNT;
    }
  }

  private synchronized Map<Message, AtomicInteger> clearMessages() {
    var prev = histogram;
    histogram = null;
    return prev;
  }

  private void dumpMessages() {
    var prev = clearMessages();
    if (prev == null) {
      return;
    }
    var sb = new StringBuilder();
    sb.append("\n======== Interop JVM Messages Chart ========\n");
    prev.entrySet().stream()
        .sorted(
            (a, b) -> {
              return b.getValue().intValue() - a.getValue().intValue();
            })
        .limit(10)
        .forEach(
            (e) -> {
              sb.append("%8d %s\n".formatted(e.getValue().intValue(), e.getKey()));
            });
    var logger = System.getLogger("org.enso.jvm.interop");
    logger.log(System.Logger.Level.ERROR, sb);
  }

  final void profileMessage(Message message, Object[] args) {
    if (DUMP_MESSAGES_COUNT >= 0) {
      incrementMessage(message);
    }
  }
}
