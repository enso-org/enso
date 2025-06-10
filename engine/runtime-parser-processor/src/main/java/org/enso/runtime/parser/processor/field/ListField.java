package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

/** Represents a {@code scala.collection.immutable.List} field in the IR node. */
public final class ListField extends Field {

  private final boolean nullable;

  ListField(String name, boolean nullable, TypeMirror type, ProcessingEnvironment procEnv) {
    super(type, name, procEnv);
    this.nullable = nullable;
  }

  @Override
  public boolean isList() {
    return true;
  }

  @Override
  public boolean isChild() {
    return true;
  }

  @Override
  public boolean isNullable() {
    return nullable;
  }
}
