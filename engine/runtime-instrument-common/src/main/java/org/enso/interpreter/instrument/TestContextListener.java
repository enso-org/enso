package org.enso.interpreter.instrument;

import com.oracle.truffle.api.TruffleContext;
import com.oracle.truffle.api.instrumentation.ContextsListener;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.instrumentation.TruffleInstrument.Registration;
import com.oracle.truffle.api.nodes.LanguageInfo;

@Registration(
    id = TestContextListener.ID,
    services = TestContextListener.class
)
public class TestContextListener extends TruffleInstrument implements ContextsListener {

  public static final String ID = "test-context-listener";


  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    env.getInstrumenter().attachContextsListener(this, true);
  }

  @Override
  public void onContextCreated(TruffleContext context) {
    System.out.println("Context created: " + ctxToStr(context));
  }

  @Override
  public void onLanguageContextCreated(TruffleContext context, LanguageInfo language) {
    System.out.println("Language context created: context=" + ctxToStr(context) + ", language="
        + language.getId());
  }

  @Override
  public void onLanguageContextInitialized(TruffleContext context, LanguageInfo language) {
    System.out.println("Language context initialized: context=" + ctxToStr(context) + ", language="
        + language.getId());
  }

  @Override
  public void onLanguageContextFinalized(TruffleContext context, LanguageInfo language) {
    System.out.println("Language context finalized: context=" + ctxToStr(context) + ", language="
        + language.getId());
  }

  @Override
  public void onLanguageContextDisposed(TruffleContext context, LanguageInfo language) {
    System.out.println("Language context disposed: context=" + ctxToStr(context) + ", language="
        + language.getId());
  }

  @Override
  public void onContextClosed(TruffleContext context) {
    System.out.println("Context closed: " + ctxToStr(context));
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
