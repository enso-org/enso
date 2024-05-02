package org.enso.ydoc.polyfill.web;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
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
final class Timers implements ProxyExecutable, Polyfill {

  private static final Logger log = LoggerFactory.getLogger(Timers.class);

  private static final String SET_INTERVAL = "set-interval";
  private static final String CLEAR_INTERVAL = "clear-interval";
  private static final String SET_TIMEOUT = "set-timeout";
  private static final String CLEAR_TIMEOUT = "clear-timeout";

  private static final String TIMERS_JS = "timers.js";

  private static final TimeUnit TIME_UNIT = TimeUnit.MILLISECONDS;

  private final ScheduledExecutorService scheduledExecutor =
      Executors.newSingleThreadScheduledExecutor(
          (r) -> {
            var thread = new Thread(r);
            thread.setDaemon(true);
            return thread;
          });

  private final ExecutorService executor;

  Timers(ExecutorService executor) {
    this.executor = executor;
  }

  @Override
  public void initialize(Context ctx) {
    Source timersJs = Source.newBuilder("js", Timers.class.getResource(TIMERS_JS)).buildLiteral();

    ctx.eval(timersJs).execute(this);
  }

  public Object setTimeout(Value func, long delay, Value[] args) {
    return scheduledExecutor.schedule(execute(func, args), delay, TIME_UNIT);
  }

  public Object setInterval(Value func, long delay, Value[] args) {
    return scheduledExecutor.scheduleAtFixedRate(execute(func, args), delay, delay, TIME_UNIT);
  }

  public void clearTimeout(Object actionId) {
    if (actionId instanceof Future action) {
      action.cancel(true);
    }
  }

  public void clearInterval(Object actionId) {
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
        yield setInterval(func, delay, args);
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
        yield setTimeout(func, delay, args);
      }
      case CLEAR_TIMEOUT -> {
        var timeoutId = arguments[1].asHostObject();
        clearTimeout(timeoutId);
        yield null;
      }
      default -> throw new IllegalStateException(command);
    };
  }

  private Runnable execute(Value func, Object[] args) {
    return () -> executor.execute(() -> func.executeVoid(args));
  }
}
