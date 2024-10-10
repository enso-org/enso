package org.enso.ydoc.polyfill.web;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implements the <a href="https://nodejs.org/api/events.html#class-eventtarget">EventTarget</a>
 * Node.js interface.
 */
final class EventTarget implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(EventTarget.class);

  private static final String NEW_EVENT_TARGET = "new-event-target";
  private static final String GET_EVENT_LISTENERS = "get-event-listeners";
  private static final String ADD_EVENT_LISTENER = "add-event-listener";
  private static final String REMOVE_EVENT_LISTENER = "remove-event-listener";
  private static final String DISPATCH_EVENT = "dispatch-event";

  private static final String EVENT_TARGET_JS = "event-target.js";

  @Override
  public void initialize(Context ctx) {
    Source jsSource =
        Source.newBuilder("js", getClass().getResource(EVENT_TARGET_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case NEW_EVENT_TARGET -> new EventStore(new ConcurrentHashMap<>());

      case GET_EVENT_LISTENERS -> {
        var store = arguments[1].as(EventStore.class);
        var type = arguments[2].asString();

        yield store.getEventListeners(type);
      }

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

    private final Map<String, Set<Value>> listeners;

    EventStore(Map<String, Set<Value>> listeners) {
      this.listeners = listeners;
    }

    public Value[] getEventListeners(String type) {
      return listeners.getOrDefault(type, Set.of()).toArray(new Value[0]);
    }

    public void addEventListener(String type, Value listener) {
      listeners.compute(
          type,
          (k, v) -> {
            Set<Value> set = v == null ? ConcurrentHashMap.newKeySet() : v;
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
      var eventListeners = listeners.getOrDefault(type, Set.of());

      for (Value listener : eventListeners) {
        try {
          listener.executeVoid(event);
        } catch (Exception e) {
          log.error("Error dispatching event of {} [{}] on {}", type, event, listener, e);
        }
      }
    }
  }
}
