package org.enso.runtime.parser.processor;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.DeclaredType;
import javax.lang.model.type.PrimitiveType;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleTypeVisitor14;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.utils.Utils;

/**
 * A context created for the generated class. Everything that is needed for the code generation of a
 * single class is contained in this class.
 */
public final class GeneratedClassContext {
  private final String className;
  private final List<Field> userFields;
  private final List<ClassField> allFields;
  private final List<Parameter> constructorParameters;
  private final ProcessingEnvironment processingEnvironment;
  private final ProcessedClass processedClass;

  private final ClassField diagnosticsMetaField;
  private final ClassField passDataMetaField;
  private final ClassField locationMetaField;
  private final ClassField idMetaField;

  /** Meta fields are always present in the generated class. */
  private final List<ClassField> metaFields;

  /**
   * @param className Simple name of the generated class
   * @param userFields List of user defined fields. These fields are collected from parameterless
   *     abstract methods in the interface.
   */
  GeneratedClassContext(
      String className,
      List<Field> userFields,
      ProcessingEnvironment processingEnvironment,
      ProcessedClass processedClass) {
    this.className = Objects.requireNonNull(className);
    this.userFields = Objects.requireNonNull(userFields);
    this.processingEnvironment = Objects.requireNonNull(processingEnvironment);
    this.processedClass = processedClass;
    ensureSimpleName(className);

    this.diagnosticsMetaField =
        ClassField.builder()
            .modifiers("protected")
            .type(Utils.diagnosticStorageTypeElement(processingEnvironment).asType())
            .name("diagnostics")
            .procEnv(processingEnvironment)
            .canBeNull(true)
            .build();
    this.passDataMetaField =
        ClassField.builder()
            .modifiers("protected")
            .type(Utils.metadataStorageTypeElement(processingEnvironment).asType())
            .name("passData")
            .initializer("new MetadataStorage()")
            .procEnv(processingEnvironment)
            .canBeNull(false)
            .build();
    this.locationMetaField =
        ClassField.builder()
            .modifiers("protected")
            .type(Utils.identifiedLocationTypeElement(processingEnvironment).asType())
            .name("location")
            .procEnv(processingEnvironment)
            .canBeNull(true)
            .build();
    this.idMetaField =
        ClassField.builder()
            .modifiers("protected")
            .type(Utils.uuidTypeElement(processingEnvironment).asType())
            .name("id")
            .canBeNull(true)
            .procEnv(processingEnvironment)
            .build();
    this.metaFields =
        List.of(diagnosticsMetaField, passDataMetaField, locationMetaField, idMetaField);

    this.allFields = new ArrayList<>(metaFields);
    for (var userField : userFields) {
      allFields.add(
          ClassField.builder()
              .modifiers("private final")
              .type(userField.getType())
              .name(userField.getName())
              .canBeNull(userField.isNullable() && !userField.isPrimitive())
              .procEnv(processingEnvironment)
              .build());
    }
    this.constructorParameters =
        allFields.stream()
            .map(classField -> new Parameter(classField.getType(), classField.name()))
            .toList();
  }

  private static void ensureSimpleName(String name) {
    if (name.contains(".")) {
      throw new IRProcessingException("Class name must be simple, not qualified", null);
    }
  }

  public ClassField getLocationMetaField() {
    return locationMetaField;
  }

  public ClassField getPassDataMetaField() {
    return passDataMetaField;
  }

  public ClassField getDiagnosticsMetaField() {
    return diagnosticsMetaField;
  }

  public ClassField getIdMetaField() {
    return idMetaField;
  }

  public List<Field> getUserFields() {
    return userFields;
  }

  /** Returns simple name of the class that is being generated. */
  public String getClassName() {
    return className;
  }

  public ProcessedClass getProcessedClass() {
    return processedClass;
  }

  List<ClassField> getMetaFields() {
    return metaFields;
  }

  /** Returns list of all fields in the generated class - meta field and user-defined fields. */
  public List<ClassField> getAllFields() {
    return allFields;
  }

  public ProcessingEnvironment getProcessingEnvironment() {
    return processingEnvironment;
  }

  /**
   * Returns list of parameters for the constructor of the subclass annotated with {@link
   * org.enso.runtime.parser.dsl.GenerateFields}. The list is gathered from all the fields present
   * in the generated super class.
   *
   * @see #getAllFields()
   * @return List of parameters for the constructor of the subclass. A subset of all the fields in
   *     the generated super class.
   */
  public List<ClassField> getSubclassConstructorParameters() {
    var ctor = processedClass.getCtor();
    var ctorParams = new ArrayList<ClassField>();
    for (var param : ctor.getParameters()) {
      var paramType = param.asType().toString();
      var paramName = param.getSimpleName().toString();
      var fieldsWithSameType =
          allFields.stream().filter(field -> paramType.equals(field.getTypeName())).toList();
      if (fieldsWithSameType.isEmpty()) {
        throw noMatchingFieldError(param);
      } else if (fieldsWithSameType.size() == 1) {
        ctorParams.add(fieldsWithSameType.get(0));
      } else {
        // There are multiple fields with the same type - try to match on the name
        var fieldsWithSameName =
            fieldsWithSameType.stream().filter(field -> paramName.equals(field.name())).toList();
        Utils.hardAssert(
            fieldsWithSameName.size() < 2,
            "Cannot have more than one field with the same name and type");
        if (fieldsWithSameName.isEmpty()) {
          throw noMatchingFieldError(param);
        }
        Utils.hardAssert(fieldsWithSameName.size() == 1);
        ctorParams.add(fieldsWithSameName.get(0));
      }
    }
    return ctorParams;
  }

  private String simpleTypeName(VariableElement param) {
    var paramType = param.asType();
    var typeVisitor =
        new SimpleTypeVisitor14<String, Void>() {
          @Override
          public String visitDeclared(DeclaredType t, Void unused) {
            return t.asElement().getSimpleName().toString();
          }

          @Override
          public String visitPrimitive(PrimitiveType t, Void unused) {
            return t.toString();
          }
        };
    var typeName = paramType.accept(typeVisitor, null);
    return typeName;
  }

  private IRProcessingException noMatchingFieldError(VariableElement param) {
    var paramSimpleType = simpleTypeName(param);
    var paramName = param.getSimpleName().toString();
    var errMsg =
        String.format(
            "No matching field found for parameter %s of type %s. All fields: %s",
            paramName, paramSimpleType, allFields);
    return new IRProcessingException(errMsg, param);
  }

  /** Method parameter */
  record Parameter(TypeMirror type, String name) {
    @Override
    public String toString() {
      return simpleTypeName() + " " + name;
    }

    String simpleTypeName() {
      return Utils.simpleTypeName(type);
    }
  }
}
