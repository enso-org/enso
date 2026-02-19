package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.pass.IRPass;
import scala.Option;

/**
 * The metadata information associated with the {@link TypeInferencePropagation} pass.
 *
 * @param type the type inferred for a given expression
 * @see TypeInferencePropagation
 */
public record InferredType(TypeRepresentation type) implements IRPass.IRMetadata {
  @Override
  public String metadataName() {
    return "Inferred Type";
  }

  @Override
  public IRPass.IRMetadata prepareForSerialization(CompilerContext compiler) {
    return this;
  }

  @Override
  public Option<IRPass.IRMetadata> restoreFromSerialization(CompilerContext compiler) {
    return Option.apply(this);
  }

  @Override
  public Option<IRPass.IRMetadata> duplicate() {
    // No need to deep copy the type as its an immutable structure.
    return Option.apply(new InferredType(type));
  }

  public static InferredType create(TypeRepresentation type) {
    if (type == null) {
      return null;
    }

    return new InferredType(type);
  }
}
