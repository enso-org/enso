package org.enso.compiler.pass.analyse;

import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.pass.IRPass;
import org.enso.persist.Persistable;
import scala.Option;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
@Persistable(clazz = FramePointer.class, id = 1283)
public record FramePointer(int parentLevel, int frameSlotIdx) implements FrameAnalysisMeta {

  public FramePointer {
    assert parentLevel >= 0;
    assert frameSlotIdx >= 0;
  }

  @Override
  public String metadataName() {
    return getClass().getSimpleName();
  }

  @Override
  public FramePointer prepareForSerialization(CompilerContext compiler) {
    return this;
  }

  @Override
  public Option<IRPass.IRMetadata> restoreFromSerialization(CompilerContext compiler) {
    return Option.apply(this);
  }

  @Override
  public Option<IRPass.IRMetadata> duplicate() {
    return Option.apply(this);
  }
}
