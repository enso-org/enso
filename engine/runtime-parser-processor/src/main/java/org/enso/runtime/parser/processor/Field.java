package org.enso.runtime.parser.processor;

import javax.lang.model.element.TypeElement;

final class Field {
  private final TypeElement type;

  /** Name of the field (identifier). */
  private final String name;

  /** If the field can be {@code null}. */
  private final boolean nullable;

  private final boolean isChild;

  Field(TypeElement type, String name, boolean nullable, boolean isChild) {
    this.type = type;
    this.name = name;
    this.nullable = nullable;
    this.isChild = isChild;
  }

  boolean isChild() {
    return isChild;
  }

  boolean isNullable() {
    return nullable;
  }

  String getName() {
    return name;
  }

  String getSimpleTypeName() {
    return type.getSimpleName().toString();
  }

  String getQualifiedTypeName() {
    return type.getQualifiedName().toString();
  }
}
