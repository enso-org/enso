package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

final class PrimitiveField extends Field {

  PrimitiveField(TypeMirror type, String name, ProcessingEnvironment procEnv) {
    super(type, name, procEnv);
  }

  @Override
  public boolean isChild() {
    return false;
  }

  @Override
  public boolean isNullable() {
    return false;
  }

  @Override
  public boolean isPrimitive() {
    return true;
  }
}
