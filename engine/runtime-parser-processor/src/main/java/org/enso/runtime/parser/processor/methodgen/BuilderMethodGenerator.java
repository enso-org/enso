package org.enso.runtime.parser.processor.methodgen;

import java.util.stream.Collectors;
import org.enso.runtime.parser.processor.ClassField;
import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.utils.Utils;

/**
 * Code generator for builder. Builder is a nested static class inside the generated class. Builder
 * has a validation code that is invoked in {@code build()} method that ensures that all the
 * required fields are set. Builder has a copy constructor - a constructor that takes the generated
 * class object and prefills all the fields with the values from the object. This copy constructor
 * is called from either the {@code duplicate} method or from copy methods.
 */
public class BuilderMethodGenerator {
  private final GeneratedClassContext generatedClassContext;

  public BuilderMethodGenerator(GeneratedClassContext generatedClassContext) {
    this.generatedClassContext = generatedClassContext;
  }

  public String generateBuilder() {
    var fieldDeclarations =
        generatedClassContext.getAllFields().stream()
            .map(
                field -> {
                  var initializer = field.initializer() != null ? " = " + field.initializer() : "";
                  return "private $type $name $initializer;"
                      .replace("$type", field.getTypeName())
                      .replace("$name", field.name())
                      .replace("$initializer", initializer);
                })
            .collect(Collectors.joining(System.lineSeparator()));

    var fieldSetters =
        generatedClassContext.getAllFields().stream()
            .map(
                field ->
                    """
                    public Builder $fieldName($fieldType $fieldName) {
                      this.$fieldName = $fieldName;
                      return this;
                    }
                    """
                        .replace("$fieldName", field.name())
                        .replace("$fieldType", field.getTypeName()))
            .collect(Collectors.joining(System.lineSeparator()));

    // Validation code for all non-nullable user fields
    var validationCode =
        generatedClassContext.getUserFields().stream()
            .filter(field -> !field.isNullable() && !field.isPrimitive())
            .map(
                field ->
                    """
                    if (this.$fieldName == null) {
                      throw new IllegalArgumentException("$fieldName is required");
                    }
                    """
                        .replace("$fieldName", field.getName()))
            .collect(Collectors.joining(System.lineSeparator()));

    var code =
        """
        public static final class Builder {
        $fieldDeclarations

          Builder() {}

        $copyConstructor

        $fieldSetters

        $buildMethod

          private void validate() {
        $validationCode
          }
        }
        """
            .replace("$fieldDeclarations", Utils.indent(fieldDeclarations, 2))
            .replace("$copyConstructor", Utils.indent(copyConstructor(), 2))
            .replace("$fieldSetters", Utils.indent(fieldSetters, 2))
            .replace("$buildMethod", Utils.indent(buildMethod(), 2))
            .replace("$validationCode", Utils.indent(validationCode, 4));
    return code;
  }

  private String copyConstructor() {
    var sb = new StringBuilder();
    var docs =
        """
        /**
         * Copy builder. Creates a <i>shallow</i> copy of the given object.
         *
         * <p>Using this copy builder is equivalent to calling {@code copy} method,
         * with appropriate parameters set to different values.
         *
         * <p>For more information about the copy semantics, see
         * {@link org.enso.compiler.core.IR}.
         *
         * @param obj the object from which the <emph>shallow</emph> copy is made.
         */
        """;
    sb.append(docs);
    sb.append("Builder(")
        .append(generatedClassContext.getProcessedClassName())
        .append(" obj) {")
        .append(System.lineSeparator());
    var superClass = generatedClassContext.getProcessedClass().getClazz().getSuperclass();
    // Meta fields are accessed directly.
    for (var metaField : generatedClassContext.getMetaFields()) {
      sb.append("  ")
          .append("this.")
          .append(metaField.name())
          .append(" = ((")
          .append(superClass)
          .append(")obj).")
          .append(metaField.name())
          .append(";")
          .append(System.lineSeparator());
    }
    // Most user fields are accessed via getters
    for (var userField : generatedClassContext.getUserFields()) {
      sb.append("  ")
          .append("this.")
          .append(userField.getName())
          .append(" = obj.")
          .append(userField.getName())
          .append("();")
          .append(System.lineSeparator());
    }
    sb.append("}");
    return sb.toString();
  }

  private String buildMethod() {
    var sb = new StringBuilder();
    var processedClassName = generatedClassContext.getProcessedClassName();
    var ctorParams = generatedClassContext.getSubclassConstructorParameters();
    var ctorParamsStr = ctorParams.stream().map(ClassField::name).collect(Collectors.joining(", "));
    var fieldsNotInCtor = Utils.diff(generatedClassContext.getAllFields(), ctorParams);
    sb.append("public ")
        .append(processedClassName)
        .append(" build() {")
        .append(System.lineSeparator());
    sb.append("  ").append("validate();").append(System.lineSeparator());
    sb.append("  ")
        .append(processedClassName)
        .append(" _result = new ")
        .append(processedClassName)
        .append("(")
        .append(ctorParamsStr)
        .append(");")
        .append(System.lineSeparator());
    var superClassType = generatedClassContext.getSuperClass();
    for (var fieldNotInCtor : fieldsNotInCtor) {
      sb.append("  ((")
          .append(superClassType.getSimpleName())
          .append(")_result).")
          .append(fieldNotInCtor.name())
          .append(" = ")
          .append(fieldNotInCtor.name())
          .append(";")
          .append(System.lineSeparator());
    }
    sb.append("  ").append("return _result;").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }
}
