package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.Message;
import com.oracle.truffle.api.nodes.Node;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.stream.Stream;
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
            obj -> {
              return OtherJvmObject.readResolve(
                  (Channel<OtherJvmPool>) channel, obj, this::findObject);
            });
    var withReadAndWrite =
        withRead.withWriteReplace(
            obj -> {
              var prev = enter(channel.isMaster(), null);
              try {
                return OtherJvmObject.writeReplace(obj, this::registerObject);
              } finally {
                leave(channel.isMaster(), null, prev);
              }
            });
    return withReadAndWrite;
  }

  final Object enter(boolean master, Node node) {
    if (master) {
      if (onEnter != null) {
        return onEnter.apply(node);
      }
    } else {
      loader(master).ctx.enter();
    }
    return null;
  }

  final void leave(boolean master, Node node, Object prev) {
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

  private static final int DUMP_MESSAGE_STACK_SIZE = 8;

  /**
   * @GuardedBy("this")
   */
  private Map<Message, WhereAndCount> histogram;

  /**
   * @GuardedBy("this")
   */
  private int countDown;

  /**
   * @GuardedBy("this")
   */
  private long countSince;

  private synchronized void incrementMessage(Message message) {
    assert DUMP_MESSAGES_COUNT > 0;
    if (histogram == null) {
      histogram = new ConcurrentHashMap<>();
      countDown = DUMP_MESSAGES_COUNT;
      countSince = System.currentTimeMillis();
    }
    var count = histogram.computeIfAbsent(message, (ignore) -> new WhereAndCount());
    count.count++;
    if (countDown-- < 0) {
      dumpMessages();
      countDown = DUMP_MESSAGES_COUNT;
    }
  }

  private synchronized Map<Message, WhereAndCount> clearMessages(StringBuilder sb) {
    var prev = histogram;
    histogram = null;
    long took = System.currentTimeMillis() - countSince;
    sb.append("\n======== Interop JVM Messages Chart in last %d ms ========\n".formatted(took));
    return prev;
  }

  private void dumpMessages() {
    var sb = new StringBuilder();
    var prev = clearMessages(sb);
    if (prev == null) {
      return;
    }
    prev.entrySet().stream()
        .sorted(
            (a, b) -> {
              return b.getValue().count - a.getValue().count;
            })
        .limit(10)
        .forEach(
            (e) -> {
              sb.append("%8d %s\n".formatted(e.getValue().count, e.getKey()));
              Stream.of(e.getValue().getStackTrace())
                  .map(StackTraceElement::toString)
                  .dropWhile(
                      l ->
                          l.contains("org.enso.jvm.interop")
                              || l.contains("java.base")
                              || l.contains("org.graalvm.truffle"))
                  .limit(DUMP_MESSAGE_STACK_SIZE)
                  .map("          at %s\n"::formatted)
                  .forEach(sb::append);
            });
    var logger = System.getLogger("org.enso.jvm.interop");
    logger.log(System.Logger.Level.ERROR, sb);
  }

  final void profileMessage(Message message, Object[] args) {
    if (DUMP_MESSAGES_COUNT >= 0) {
      incrementMessage(message);
    }
  }

  private static final class WhereAndCount extends Exception {
    int count;
  }
}
