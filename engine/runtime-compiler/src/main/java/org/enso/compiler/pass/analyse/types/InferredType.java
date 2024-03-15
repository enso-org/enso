package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.ProcessingPass;
import scala.Option;

public record InferredType(TypeRepresentation type) implements ProcessingPass.Metadata {
  // TODO maybe later we may want to use:
  enum Origin {
    Checked,
    Literal,
    Inferred
  }

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
    return null;
  }

  public static InferredType create(TypeRepresentation type) {
    if (type == null) {
      return null;
    }

    return new InferredType(type);
  }
}
