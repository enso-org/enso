package org.enso.ydoc.polyfill.web;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.function.Predicate;

@TargetClass(
    className = Target_ExecutorsFactory.CLASS_NAME,
    onlyWith = Target_ExecutorsFactory.Available.class)
final class Target_ExecutorsFactory {
  static final String CLASS_NAME = "io.helidon.webserver.ExecutorsFactory";

  @Substitute
  private static ThreadFactory virtualThreadFactory() {
    return Thread.ofPlatform().name("Emulating loom", 0).factory();
  }

  @Substitute
  public static ExecutorService newLoomServerVirtualThreadPerTaskExecutor() {
    return Executors.newThreadPerTaskExecutor(virtualThreadFactory());
  }

  static final class Available implements Predicate {
    @Override
    public boolean test(Object t) {
      try {
        Class.forName(CLASS_NAME);
        return true;
      } catch (ClassNotFoundException e) {
        return false;
      }
    }
  }
}
