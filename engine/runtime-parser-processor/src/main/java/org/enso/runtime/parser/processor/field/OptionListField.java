package org.enso.runtime.parser.processor.field;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.processor.utils.Utils;

/** Field representing {@code Option<List<? extends IR>>}. */
public final class OptionListField extends Field {

  public OptionListField(String name, TypeMirror type, ProcessingEnvironment procEnv) {
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

  /** Returns the nested type arg */
  public TypeElement getNestedTypeParameter() {
    var listTypeArg = Utils.getTypeArgument(type);
    var typeArg = Utils.getTypeArgument(listTypeArg);
    return (TypeElement) procEnv.getTypeUtils().asElement(typeArg);
  }
}
