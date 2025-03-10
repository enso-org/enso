package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

final class ReferenceField extends Field {
  private final boolean nullable;
  private final boolean isChild;

  ReferenceField(
      ProcessingEnvironment procEnv,
      TypeMirror type,
      String name,
      boolean nullable,
      boolean isChild) {
    super(type, name, procEnv);
    this.nullable = nullable;
    this.isChild = isChild;
  }

  @Override
  public boolean isChild() {
    return isChild;
  }

  @Override
  public boolean isNullable() {
    return nullable;
  }
}
