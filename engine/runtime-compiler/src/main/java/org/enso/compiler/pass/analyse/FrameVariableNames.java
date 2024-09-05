package org.enso.compiler.pass.analyse;

import java.util.List;
import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.persist.Persistable;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

@Persistable(id = 1286)
public final class FrameVariableNames implements FrameAnalysisMeta {
  private final List<String> names;

  public FrameVariableNames(List<String> variableNames) {
    this.names = variableNames;
  }

  public static FrameVariableNames create(scala.collection.immutable.List<String> names) {
    return new FrameVariableNames(CollectionConverters.asJava(names));
  }

  public List<String> variableNames() {
    return names;
  }

  @Override
  public String metadataName() {
    return getClass().getSimpleName();
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
    return Option.apply(new FrameVariableNames(names));
  }
}
