package org.enso.interpreter.instrument;

import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.instrumentation.ThreadsActivationListener;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.instrumentation.TruffleInstrument.Registration;

@Registration(id = TestThreadListener.ID, services = TestThreadListener.class)
public class TestThreadListener extends TruffleInstrument implements ThreadsActivationListener {

  public static final String ID = "test-thread-listener";

  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    env.getInstrumenter().attachThreadsActivationListener(this);
  }

  @Override
  public void onEnterThread(TruffleContext context) {
    System.out.printf(
        "[TestThreadListener:%s] onEnterThread: ctx=%s%n",
        Thread.currentThread().getName(), ctxToStr(context));
  }

  @Override
  public void onLeaveThread(TruffleContext context) {
    System.out.printf(
        "[TestThreadListener:%s] onLeaveThread: ctx=%s%n",
        Thread.currentThread().getName(), ctxToStr(context));
  }

  private static String ctxToStr(TruffleContext ctx) {
    try {
      var field = ctx.getClass().getDeclaredField("polyglotContext");
      field.setAccessible(true);
      var fieldVal = field.get(ctx);
      return fieldVal.toString();
    } catch (NoSuchFieldException | IllegalAccessException e) {
      throw new AssertionError(e);
    }
  }
}
