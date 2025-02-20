package org.enso.ydoc.server.nativeimage;

import com.oracle.svm.core.annotate.Alias;
import com.oracle.svm.core.annotate.RecomputeFieldValue;
import com.oracle.svm.core.annotate.TargetClass;
import io.helidon.common.LazyValue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.function.Supplier;

@TargetClass(className = "io.helidon.webclient.api.LoomClient")
final class Target_LoomClient {

  @Alias
  @RecomputeFieldValue(kind = RecomputeFieldValue.Kind.FromAlias)
  static LazyValue<ExecutorService> EXECUTOR = LazyValue.create(new ExecutorSupplier());

  private static final class ExecutorSupplier implements Supplier<ExecutorService> {
    @Override
    public ExecutorService get() {
      return Executors.newThreadPerTaskExecutor(
          Thread.ofPlatform().name("emulating-loom-helidon-client-", 0).factory());
    }
  }
}
