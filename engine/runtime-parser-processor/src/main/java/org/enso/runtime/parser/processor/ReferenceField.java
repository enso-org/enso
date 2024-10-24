package org.enso.runtime.parser.processor;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;

final class ReferenceField implements Field {
  private final ProcessingEnvironment procEnv;
  private final TypeElement type;
  private final String name;
  private final boolean nullable;
  private final boolean isChild;

  ReferenceField(
      ProcessingEnvironment procEnv,
      TypeElement type,
      String name,
      boolean nullable,
      boolean isChild) {
    this.procEnv = procEnv;
    this.type = type;
    this.name = name;
    this.nullable = nullable;
    this.isChild = isChild;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String getSimpleTypeName() {
    return type.toString();
  }

  @Override
  public String getQualifiedTypeName() {
    return type.getQualifiedName().toString();
  }

  @Override
  public boolean isChild() {
    return isChild;
  }

  @Override
  public boolean isPrimitive() {
    return false;
  }

  @Override
  public boolean isNullable() {
    return nullable;
  }

  @Override
  public boolean isExpression() {
    return Utils.isSubtypeOfExpression(type.asType(), procEnv);
  }
}
