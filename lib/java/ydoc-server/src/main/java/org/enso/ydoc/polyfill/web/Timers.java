package org.enso.ydoc.polyfill.web;

import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import org.enso.ydoc.Polyfill;
import org.enso.ydoc.polyfill.Arguments;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.proxy.ProxyExecutable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Implements the <a href="https://nodejs.org/api/timers.html">Timers</a> Node.js API. */
final class Timers implements Polyfill, ProxyExecutable {

  private static final Logger log = LoggerFactory.getLogger(Timers.class);

  private static final String SET_INTERVAL = "set-interval";
  private static final String CLEAR_INTERVAL = "clear-interval";
  private static final String SET_TIMEOUT = "set-timeout";
  private static final String CLEAR_TIMEOUT = "clear-timeout";

  private static final String TIMERS_JS = "timers.js";

  private static final TimeUnit TIME_UNIT = TimeUnit.MILLISECONDS;
  private static final long MINIMUM_DELAY = 1;
  private static final long MAXIMUM_DELAY = 2147483647;

  private final ScheduledExecutorService executor;

  Timers(ScheduledExecutorService executor) {
    this.executor = executor;
  }

  @Override
  public void initialize(Context ctx) {
    Source jsSource = Source.newBuilder("js", getClass().getResource(TIMERS_JS)).buildLiteral();

    ctx.eval(jsSource).execute(this);
  }

  private Future<?> setTimeout(Value func, long delay, Object[] args) {
    return executor.schedule(() -> func.executeVoid(args), delay, TIME_UNIT);
  }

  private Future<?> setInterval(Value func, long delay, Object[] args) {
    return executor.scheduleAtFixedRate(() -> func.executeVoid(args), delay, delay, TIME_UNIT);
  }

  private void clearTimeout(Object actionId) {
    if (actionId instanceof Future<?> action) {
      action.cancel(true);
    }
  }

  private void clearInterval(Object actionId) {
    clearTimeout(actionId);
  }

  @Override
  public Object execute(Value... arguments) {
    String command = arguments[0].asString();

    log.debug(Arguments.toString(arguments));

    return switch (command) {
      case SET_INTERVAL -> {
        var func = arguments[1];
        var delay = arguments[2].asLong();
        var args = arguments[3].as(Value[].class);
        yield setInterval(func, normalizedDelay(delay), args);
      }
      case CLEAR_INTERVAL -> {
        var intervalId = arguments[1].asHostObject();
        clearInterval(intervalId);
        yield null;
      }
      case SET_TIMEOUT -> {
        var func = arguments[1];
        var delay = arguments[2].asLong();
        var args = arguments[3].as(Value[].class);
        yield setTimeout(func, normalizedDelay(delay), args);
      }
      case CLEAR_TIMEOUT -> {
        var timeoutId = arguments[1].asHostObject();
        clearTimeout(timeoutId);
        yield null;
      }
      default -> throw new IllegalStateException(command);
    };
  }

  /**
   * Applies Node.js constraints on a delay value.
   *
   * @param delay the input delay value.
   * @return the delay value bounded to the permitted range.
   */
  private long normalizedDelay(long delay) {
    return Math.min(MAXIMUM_DELAY, Math.max(MINIMUM_DELAY, delay));
  }
}
