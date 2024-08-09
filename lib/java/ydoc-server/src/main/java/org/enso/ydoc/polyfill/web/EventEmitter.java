package org.enso.ydoc.polyfill.web;

import java.util.Arrays;
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
 * Implements the <a href="https://nodejs.org/api/events.html#class-eventemitter">EventEmitter</a>
 * Node.js interface.
 */
final class EventEmitter implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(EventEmitter.class);

  private static final String NEW_EVENT_STORE = "new-event-store";
  private static final String GET_LISTENERS = "get-listeners";
  private static final String ADD_LISTENER = "add-listener";
  private static final String REMOVE_LISTENER = "remove-listener";
  private static final String EMIT = "emit";

  private static final String EVENT_EMITTER_JS = "event-emitter.js";

  @Override
  public void initialize(Context ctx) {
    Source jsSource =
        Source.newBuilder("js", getClass().getResource(EVENT_EMITTER_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case NEW_EVENT_STORE -> new Store(new ConcurrentHashMap<>());

      case GET_LISTENERS -> {
        var store = arguments[1].as(Store.class);
        var eventName = arguments[2].asString();

        yield store.getListeners(eventName);
      }

      case ADD_LISTENER -> {
        var store = arguments[1].as(Store.class);
        var eventName = arguments[2].asString();
        var listener = arguments[3];

        store.addListener(eventName, listener);
        yield null;
      }

      case REMOVE_LISTENER -> {
        var store = arguments[1].as(Store.class);
        var eventName = arguments[2].asString();
        var listener = arguments[3];

        store.removeListener(eventName, listener);
        yield null;
      }

      case EMIT -> {
        var store = arguments[1].as(Store.class);
        var eventName = arguments[2].asString();
        var args = arguments[3].as(Value[].class);

        store.emit(eventName, args);
        yield null;
      }

      default -> throw new IllegalStateException(command);
    };
  }

  private static final class Store {

    private final Map<String, Set<Value>> listeners;

    Store(Map<String, Set<Value>> listeners) {
      this.listeners = listeners;
    }

    public Value[] getListeners(String type) {
      return listeners.getOrDefault(type, Set.of()).toArray(new Value[0]);
    }

    public void addListener(String type, Value listener) {
      listeners.compute(
          type,
          (k, v) -> {
            Set<Value> set = v == null ? ConcurrentHashMap.newKeySet() : v;
            set.add(listener);
            return set;
          });
    }

    public void removeListener(String type, Value listener) {
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

    public void emit(String type, Object[] args) {
      listeners
          .getOrDefault(type, Set.of())
          .forEach(
              listener -> {
                try {
                  listener.executeVoid(args);
                } catch (Exception e) {
                  log.error(
                      "Error emitting event of {} [{}] on {}",
                      type,
                      Arrays.toString(args),
                      listener,
                      e);
                }
              });
    }
  }
}
