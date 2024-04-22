package org.enso.ydoc.polyfill.web;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.enso.ydoc.Polyfill;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;

/**
 * Implements the <a href="https://nodejs.org/api/events.html#class-eventemitter">EventEmitter</a>
 * Node.js interface.
 */
final class EventEmitter implements ProxyExecutable, Polyfill {

  private static final String NEW_EVENT_STORE = "new-event-store";
  private static final String GET_LISTENERS = "get-listeners";
  private static final String ADD_LISTENER = "add-listener";
  private static final String REMOVE_LISTENER = "remove-listener";
  private static final String EMIT = "emit";

  private static final String EVENT_EMITTER_JS = "event-emitter.js";

  EventEmitter() {}

  @Override
  public void initialize(Context ctx) {
    Source eventEmitterJs =
        Source.newBuilder("js", EventEmitter.class.getResource(EVENT_EMITTER_JS)).buildLiteral();

    ctx.eval(eventEmitterJs).execute(this);
  }

  @Override
  public Object execute(Value... arguments) {
    var command = arguments[0].asString();
    System.err.println(command + " " + Arrays.toString(arguments));

    return switch (command) {
      case NEW_EVENT_STORE -> new Store(new HashMap<>());

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
      return listeners.getOrDefault(type, new HashSet<>()).toArray(new Value[0]);
    }

    public void addListener(String type, Value listener) {
      listeners.compute(
          type,
          (k, v) -> {
            var set = v == null ? new HashSet<Value>() : v;
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
                  System.err.println(
                      "Error emitting event of "
                          + type
                          + " ["
                          + Arrays.toString(args)
                          + "] on "
                          + listener
                          + " "
                          + e);
                  e.printStackTrace(System.err);
                }
              });
    }
  }
}
