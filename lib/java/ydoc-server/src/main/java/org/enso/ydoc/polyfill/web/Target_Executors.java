package org.enso.ydoc.polyfill.web;

import com.oracle.svm.core.annotate.Alias;
import com.oracle.svm.core.annotate.Substitute;
import com.oracle.svm.core.annotate.TargetClass;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

@TargetClass(className = "io.helidon.webserver.ServerListener")
final class Target_ServerListener {
  @Substitute
  private static ThreadFactory virtualThreadFactory() {
    return Thread.ofPlatform().factory();
  }
}

@TargetClass(className = "io.helidon.webserver.ThreadPerTaskExecutor")
final class HelidonExecutor {
  @Alias
  HelidonExecutor(ThreadFactory factory) {}

  @Substitute
  public static HelidonExecutor create(ThreadFactory ignore) {
    return new HelidonExecutor(Thread.ofPlatform().factory());
  }
}

@TargetClass(Executors.class)
final class Target_Executors {
  @Substitute
  public static ExecutorService newVirtualThreadPerTaskExecutor() {
    ThreadFactory factory = Thread.ofPlatform().name("Emulating loom", 0).factory();
    return Executors.newThreadPerTaskExecutor(factory);
  }
}
