package org.enso.interpreter.caches;

import java.io.IOException;
import java.util.UUID;
import java.util.function.Function;

import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.ProcessingPass;

final class CacheUtils {
  private CacheUtils() {
  }

  static Function<Object, Object> writeReplace(CompilerContext context) {
    return (obj) -> switch (obj) {
      case ProcessingPass.Metadata metadata -> metadata.prepareForSerialization(context);
      case UUID _ -> null;
      case null -> null;
      default -> obj;
    };
  }

  static Function<Object, Object> readResolve(CompilerContext context) {
    return (obj) -> switch (obj) {
      case ProcessingPass.Metadata metadata -> {
        var option = metadata.restoreFromSerialization(context);
        if (option.nonEmpty()) {
          yield option.get();
        } else {
          throw raise(RuntimeException.class, new IOException("Cannot convert " + metadata));
        }
      }
      case null -> null;
      default -> obj;
    };
  }

  @SuppressWarnings("unchecked")
  static <T extends Exception> T raise(Class<T> cls, Exception e) throws T {
    throw (T) e;
  }

}
