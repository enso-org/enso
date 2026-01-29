package org.enso.compiler.pass.analyse;

import java.util.List;
import java.util.Objects;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.pass.IRPass;
import org.enso.persist.Persistable;
import scala.Option;

@Persistable(id = 1286)
public final class FrameVariableNames implements FrameAnalysisMeta {
  private final List<String> names;

  FrameVariableNames(List<String> variableNames) {
    this.names = variableNames;
  }

  public static FrameVariableNames create(java.util.List<String> names) {
    return new FrameVariableNames(names);
  }

  public List<String> variableNames() {
    return names;
  }

  @Override
  public String metadataName() {
    return getClass().getSimpleName();
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
    return Option.apply(new FrameVariableNames(names));
  }

  @Override
  public int hashCode() {
    int hash = 3;
    hash = 59 * hash + Objects.hashCode(this.names);
    return hash;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    final FrameVariableNames other = (FrameVariableNames) obj;
    return Objects.equals(this.names, other.names);
  }

  @Override
  public String toString() {
    return "FrameVariableNames{" + "names=" + names + '}';
  }
}
