package org.enso.compiler.pass.analyse;

import java.util.UUID;
import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.persist.Persistable;
import scala.Option;

/**
 * A representation of a pointer into a stack frame at a given number of levels above the current.
 */
@Persistable(clazz = FramePointer.class, id = 1288)
public record FramePointer(int parentLevel, int frameSlotIdx, UUID externalId, UUID internalId)
    implements FrameAnalysisMeta {

  public FramePointer {
    assert parentLevel >= 0;
    assert frameSlotIdx >= 0;
  }

  public FramePointer(int parentLevel, int frameSlotIdx) {
    this(parentLevel, frameSlotIdx, null, null);
  }

  public FramePointer(int parentLevel, int frameSlotIdx, UUID internalId) {
    this(parentLevel, frameSlotIdx, null, internalId);
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
    return Option.apply(this);
  }
}
