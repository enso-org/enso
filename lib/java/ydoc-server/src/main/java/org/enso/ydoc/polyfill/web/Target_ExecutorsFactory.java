package org.enso.ydoc.polyfill.web;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

@TargetClass(className = "io.helidon.webserver.ExecutorsFactory")
final class Target_ExecutorsFactory {
  @Substitute
  private static ThreadFactory virtualThreadFactory() {
    return Thread.ofPlatform().name("Emulating loom", 0).factory();
  }

  @Substitute
  public static ExecutorService newLoomServerVirtualThreadPerTaskExecutor() {
    return Executors.newThreadPerTaskExecutor(virtualThreadFactory());
  }
}
