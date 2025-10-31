package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
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
  /**
   * @GuardedBy("this")
   */
  private long idCounter;

  private final Map<Long, TruffleObject> objectsById = new HashMap<>();
  private final Map<TruffleObject, Long> objectsToId = new HashMap<>();
  private final Map<Long, OtherJvmObject> incomming = new HashMap<>();

  /** context to use when entering tests */
  private OtherJvmLoader loader;

  private Function<Node, Object> onEnter;
  private BiConsumer<Node, Object> onLeave;
  private Class<? extends TruffleLanguage> language;

  /** Master Channel can be associated with actions on enter and on leave. */
  public final void onEnterLeave(
      Class<? extends TruffleLanguage> lang,
      Function<Node, Object> onEnter,
      BiConsumer<Node, Object> onLeave) {
    this.language = lang;
    this.onEnter = onEnter;
    this.onLeave = onLeave;
  }

  final boolean hasLanguage() {
    return language != null;
  }

  final Class<? extends TruffleLanguage> getLanguage() throws UnsupportedMessageException {
    var l = language;
    if (l == null) {
      throw UnsupportedMessageException.create();
    } else {
      return l;
    }
  }

  /**
   * Registers an instance of Truffle interop object before sending it to the "other JVM". The
   * system can lookup existing ID for the object - useful for sharing IDs/instances of
   * <em>immutable objects</em> like {@link Class}es.
   *
   * @param obj the object to find ID for
   * @param cacheIds should the IDs be cached
   * @return identification ID that can be fed into {@link #findObject} later
   * @see #findObject
   */
  private synchronized long registerObject(TruffleObject obj, boolean cacheIds) {
    assert !(obj instanceof OtherJvmObject)
        : "It should be real truffle object, not just a proxy: " + obj;
    var id = cacheIds ? objectsToId.get(obj) : null;
    if (id == null) {
      id = ++idCounter;
      objectsById.put(id, obj);
      if (cacheIds) {
        objectsToId.put(obj, id);
      }
    }
    return id;
  }

  /**
   * Looks an object registered by {@link #registerObject} up.
   *
   * @param id the ID to look up
   * @return object with assigned ID or {@code null}
   */
  final synchronized TruffleObject findObject(long id) {
    return objectsById.get(id);
  }

  final synchronized void gc(long id) {
    var prev = objectsById.remove(id);
    assert prev != null : dumpIds("Each id is removed only once, but " + id);
  }

  private String dumpIds(String msg) {
    var sb = new StringBuilder();
    sb.append(msg);
    for (var e : objectsById.entrySet()) {
      sb.append("\n  " + e.getKey() + " => " + e.getValue());
    }
    for (var e : objectsToId.entrySet()) {
      sb.append("\n  " + e.getKey() + " #" + e.getValue());
    }
    return sb.toString();
  }

  private final synchronized OtherJvmObject findCached(OtherJvmObject withId) {
    var existing = incomming.get(withId.id());
    if (existing == null) {
      incomming.put(withId.id(), withId);
      return withId;
    } else {
      return existing;
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public final Persistance.Pool createPool(Channel<?> channel) {
    var withRead =
        Persistables.POOL.withReadResolve(
            obj -> {
              return OtherJvmObject.readResolve(
                  (Channel<OtherJvmPool>) channel, obj, this::findObject, this::findCached);
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

  void findLibraries(boolean master, TruffleObject file) {
    loader(master).findLibraries(file);
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
  private int countMessages;

  /**
   * @GuardedBy("this")
   */
  private long countSince;

  private synchronized void incrementMessage(Message message) {
    assert DUMP_MESSAGES_COUNT > 0;
    if (histogram == null) {
      histogram = new ConcurrentHashMap<>();
      countMessages = 0;
      countSince = System.currentTimeMillis();
    }
    var count = histogram.computeIfAbsent(message, (ignore) -> new WhereAndCount());
    count.count++;
    if (++countMessages >= DUMP_MESSAGES_COUNT) {
      var logger = System.getLogger("org.enso.jvm.interop");
      logger.log(System.Logger.Level.ERROR, dumpMessages());
      countMessages = 0;
    }
  }

  private synchronized Map<Message, WhereAndCount> clearMessages(StringBuilder sb) {
    var prev = histogram;
    histogram = null;
    long took = System.currentTimeMillis() - countSince;
    sb.append("\n======== Interop JVM Messages Chart in last %d ms ========\n".formatted(took));
    return prev;
  }

  private String dumpMessages() {
    var sb = new StringBuilder();
    var prev = clearMessages(sb);
    if (prev == null) {
      return sb.toString();
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
    return sb.toString();
  }

  final void profileMessage(Message message, Object[] args) {
    if (DUMP_MESSAGES_COUNT >= 0) {
      incrementMessage(message);
    }
  }

  final void assertMessagesCount(String msg, int cnt, Runnable run) {
    assert DUMP_MESSAGES_COUNT == Integer.MAX_VALUE;
    clearMessages(new StringBuilder());
    countMessages = 0;
    run.run();
    if (countMessages > cnt) {
      var txt =
          msg
              + ", expected at most "
              + cnt
              + " messages, but was "
              + countMessages
              + dumpMessages();
      throw new AssertionError(txt);
    }
  }

  private static final class WhereAndCount extends Exception {
    int count;
  }
}
