package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

/** Field with type {@code org.enso.persist.Persistance.Reference}. */
public final class PersistanceReferenceField extends Field {
  protected PersistanceReferenceField(String name, TypeMirror type, ProcessingEnvironment procEnv) {
    super(type, name, procEnv);
  }

  @Override
  public boolean isChild() {
    return true;
  }

  @Override
  public boolean isNullable() {
    return false;
  }
}
