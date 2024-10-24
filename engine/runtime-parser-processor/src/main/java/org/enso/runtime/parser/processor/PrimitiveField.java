package org.enso.runtime.parser.processor;

import javax.lang.model.type.TypeMirror;

final class PrimitiveField implements Field {

  private final TypeMirror type;
  private final String name;

  PrimitiveField(TypeMirror type, String name) {
    this.type = type;
    this.name = name;
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
    return null;
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

  @Override
  public boolean isExpression() {
    return false;
  }
}
