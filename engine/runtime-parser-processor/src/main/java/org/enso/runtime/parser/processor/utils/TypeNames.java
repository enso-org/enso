package org.enso.runtime.parser.processor.utils;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.processor.ClassField;
import org.enso.runtime.parser.processor.field.Field;

/** Utility class that returns either qualified or simple type names. */
public final class TypeNames {
  private final boolean shouldUseFQN;

  public TypeNames(boolean shouldUseFQN) {
    this.shouldUseFQN = shouldUseFQN;
  }

  public String typeName(Element element) {
    if (element instanceof TypeElement typeElement) {
      return typeName(typeElement);
    } else {
      return element.getSimpleName().toString();
    }
  }

  public String typeName(TypeElement typeElement) {
    if (shouldUseFQN) {
      return typeElement.getQualifiedName().toString();
    } else {
      return typeElement.getSimpleName().toString();
    }
  }

  public String typeName(TypeMirror typeMirror) {
    if (shouldUseFQN) {
      return Utils.qualifiedTypeName(typeMirror);
    } else {
      return Utils.simpleTypeName(typeMirror);
    }
  }

  public String typeName(Field field) {
    return typeName(field.getType());
  }

  public String typeName(ClassField classField) {
    return typeName(classField.getType());
  }
}
