package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

/** Field representing {@code scala.Option} */
public final class OptionField extends Field {

  public OptionField(String name, TypeMirror type, ProcessingEnvironment procEnv) {
    super(type, name, procEnv);
  }

  @Override
  public boolean isOption() {
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
