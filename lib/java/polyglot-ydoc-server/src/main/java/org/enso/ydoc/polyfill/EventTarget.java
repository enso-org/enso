package org.enso.ydoc.polyfill;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

final class EventTarget implements ProxyExecutable, Polyfill {

  private static final String NEW_EVENT_TARGET = "new-event-target";
  private static final String ADD_EVENT_LISTENER = "add-event-listener";
  private static final String REMOVE_EVENT_LISTENER = "remove-event-listener";
  private static final String DISPATCH_EVENT = "dispatch-event";

  private static final String EVENT_TARGET_JS = "event-target.js";

  private final ExecutorService executor;

  EventTarget(ExecutorService executor) {
    this.executor = executor;
  }

  @Override
  public void initialize(Context ctx) {
    Source eventTargetJs =
        Source.newBuilder("js", EventTarget.class.getResource(EVENT_TARGET_JS)).buildLiteral();

    ctx.eval(eventTargetJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();
    System.err.println(command + " " + Arrays.toString(arguments));

    return switch (command) {
      case NEW_EVENT_TARGET -> new EventStore(executor, new HashMap<>());

      case ADD_EVENT_LISTENER -> {
        var store = arguments[1].as(EventStore.class);
        var type = arguments[2].asString();
        var listener = arguments[3];

        store.addEventListener(type, listener);
        yield null;
      }

      case REMOVE_EVENT_LISTENER -> {
        var store = arguments[1].as(EventStore.class);
        var type = arguments[2].asString();
        var listener = arguments[3];

        store.removeEventListener(type, listener);
        yield null;
      }

      case DISPATCH_EVENT -> {
        var store = arguments[1].as(EventStore.class);
        var type = arguments[2].asString();
        var event = arguments[3];

        store.dispatchEvent(type, event);
        yield null;
      }

      default -> throw new IllegalStateException(command);
    };
  }

  private static final class EventStore {

    private final ExecutorService executor;
    private final Map<String, Set<Value>> listeners;

    EventStore(ExecutorService executor, Map<String, Set<Value>> listeners) {
      this.executor = executor;
      this.listeners = listeners;
    }

    public void addEventListener(String type, Value listener) {
      listeners.compute(
          type,
          (k, v) -> {
            var set = v == null ? new HashSet<Value>() : v;
            set.add(listener);
            return set;
          });
    }

    public void removeEventListener(String type, Value listener) {
      listeners.compute(
          type,
          (k, v) -> {
            if (v == null) {
              return v;
            } else {
              v.remove(listener);
              return v.isEmpty() ? null : v;
            }
          });
    }

    public void dispatchEvent(String type, Value event) {
      listeners
          .getOrDefault(type, Set.of())
          .forEach(
              listener -> {
                try {
                  listener.executeVoid(event);
                } catch (Exception e) {
                  System.err.println(
                      "Error dispatching event [" + type + "] " + listener + " " + event + " " + e);
                }
              });
    }
  }
}
