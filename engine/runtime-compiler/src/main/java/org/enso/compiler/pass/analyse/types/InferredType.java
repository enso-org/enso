package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.ProcessingPass;
import scala.Option;

/**
 * The metadata information associated with the {@link TypeInferencePropagation} pass.
 *
 * @param type the type inferred for a given expression
 * @see TypeInferencePropagation
 */
public record InferredType(TypeRepresentation type) implements ProcessingPass.Metadata {
  @Override
  public String metadataName() {
    return "Inferred Type";
  }

  @Override
  public ProcessingPass.Metadata prepareForSerialization(CompilerStub compiler) {
    return this;
  }

  @Override
  public Option<ProcessingPass.Metadata> restoreFromSerialization(CompilerStub compiler) {
    return Option.apply(this);
  }

  @Override
  public Option<ProcessingPass.Metadata> duplicate() {
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
