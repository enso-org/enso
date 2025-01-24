package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

/** Represents a {@code scala.collection.immutable.List} field in the IR node. */
final class ListField extends Field {
  ListField(String name, TypeMirror type, ProcessingEnvironment procEnv) {
    super(type, name, procEnv);
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
    return false;
  }
}
