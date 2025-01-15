package org.enso.ydoc.server.nativeimage;

import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

@TargetClass(className = "io.helidon.webserver.ExecutorsFactory")
final class Target_ExecutorsFactory {
  @Substitute
  private static ThreadFactory virtualThreadFactory() {
    return Thread.ofPlatform().name("emulating-loom-helidon-server-", 0).factory();
  }

  @Substitute
  public static ExecutorService newLoomServerVirtualThreadPerTaskExecutor() {
    return Executors.newThreadPerTaskExecutor(virtualThreadFactory());
  }
}
