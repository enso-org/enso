package org.enso.compiler;

import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.compiler.pass.IRPass;
import scala.Option;

import java.util.Optional;

/**
 * A set of helper methods for handling of IR metadata.
 * <p>
 * This encapsulates the friction of interop between Scala and Java types.
 */
public class MetadataInteropHelpers {
  public static <T> Optional<T> getOptionalMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Option<ProcessingPass.Metadata> option = ir.passData().get(pass);
    if (option.isDefined()) {
      try {
        return Optional.of(expectedType.cast(option.get()));
      } catch (ClassCastException exception) {
        throw new IllegalStateException("Unexpected metadata type " + option.get().getClass().getCanonicalName() + " " +
            "for " + pass, exception);
      }
    } else {
      return Optional.empty();
    }
  }

  public static <T> T getMetadata(IR ir, IRPass pass, Class<T> expectedType) {
    Optional<T> optional = getOptionalMetadata(ir, pass, expectedType);
    if (optional.isEmpty()) {
      throw new IllegalStateException("Missing expected " + pass + " metadata for " + ir + ".");
    }

    return optional.get();
  }

}
