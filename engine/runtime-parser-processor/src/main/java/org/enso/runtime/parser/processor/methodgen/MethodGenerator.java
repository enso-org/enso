package org.enso.runtime.parser.processor.methodgen;

import java.util.Objects;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.processor.ClassField;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.utils.TypeNames;

abstract class MethodGenerator {
  private final TypeNames typeNames;

  MethodGenerator(TypeNames typeNames) {
    this.typeNames = Objects.requireNonNull(typeNames);
  }

  String typeName(Element element) {
    return typeNames.typeName(element);
  }

  String typeName(TypeElement typeElement) {
    return typeNames.typeName(typeElement);
  }

  String typeName(TypeMirror typeMirror) {
    return typeNames.typeName(typeMirror);
  }

  String typeName(Field field) {
    return typeNames.typeName(field);
  }

  String typeName(ClassField classField) {
    return typeNames.typeName(classField);
  }
}
