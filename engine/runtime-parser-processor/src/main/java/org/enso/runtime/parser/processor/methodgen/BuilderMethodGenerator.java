package org.enso.runtime.parser.processor.methodgen;

import java.util.stream.Collectors;
import org.enso.runtime.parser.processor.ClassField;
import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.field.ListField;
import org.enso.runtime.parser.processor.field.OptionField;
import org.enso.runtime.parser.processor.field.OptionListField;
import org.enso.runtime.parser.processor.field.PersistanceReferenceField;
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
                      .replace("$type", field.getSimpleTypeName())
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
                        .replace("$fieldType", field.getSimpleTypeName()))
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
    sb.append("Builder(")
        .append(generatedClassContext.getProcessedClass().getClazz().getSimpleName())
        .append(" obj) {")
        .append(System.lineSeparator());
    var metaFieldsCopyCode = """
        this.diagnostics = obj.diagnosticsCopy();
        this.passData = obj.passData.copy();
        if (obj.location != null) {
          this.location = new IdentifiedLocation(obj.location.start(), obj.location.end(), obj.location.uuid());
        } else {
          this.location = null;
        }
        this.id = obj.id;
        """;
    sb.append(Utils.indent(metaFieldsCopyCode));
    sb.append(System.lineSeparator());
    for (var userField : generatedClassContext.getUserFields()) {
      var code = switch (userField) {
        case OptionListField optionListField -> """
            if (obj.${name}().isDefined()) {
              this.${name} = Option.apply(
                obj.${name}()
                  .get()
                  .map(ch -> ch.duplicate(true, true, true, true))
              );
            } else {
              this.${name} = Option.empty();
            }
            """
            .replace("${name}", userField.getName());
        case ListField listField -> """
            this.${name} =
              obj.${name}().map(ch -> ch.duplicate(true, true, true, true));
            """
            .replace("${name}", userField.getName());
        case OptionField optionField -> """
            if (obj.${name}().isDefined()) {
              this.${name} = Option.apply(
                obj.${name}().get().duplicate(true, true, true, true)
              );
            } else {
              this.${name} = Option.empty();
            }
            """
            .replace("${name}", userField.getName());
        case PersistanceReferenceField refField -> """
            this.${name} = Reference.of(
              obj.${name}().get(${type}.class)
            );
            """
            .replace("${name}", userField.getName())
            .replace("${type}", refField.getTypeParameter().getSimpleName());
        case Field field when field.isNullable() -> """
            if (obj.${name}() != null) {
              this.${name} = obj.${name}().duplicate(true, true, true, true);
            } else {
              this.${name} = null;
            }
            """
            .replace("${name}", userField.getName());
        case Field childField when childField.isChild() -> """
            this.${name} = obj.${name}().duplicate(true, true, true, true);
            """
            .replace("${name}", userField.getName());
        default -> """
            this.${name} = obj.${name}();
            """
            .replace("${name}", userField.getName());
      };
      sb.append("  // Copy of '")
        .append(userField.getName())
        .append("' field")
        .append(System.lineSeparator());
      sb.append(Utils.indent(code));
      sb.append(System.lineSeparator());
    }
    sb.append("}");
    return sb.toString();
  }

  private String buildMethod() {
    var sb = new StringBuilder();
    var processedClassName =
        generatedClassContext.getProcessedClass().getClazz().getSimpleName().toString();
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
        .append(" result = new ")
        .append(processedClassName)
        .append("(")
        .append(ctorParamsStr)
        .append(");")
        .append(System.lineSeparator());
    for (var fieldNotInCtor : fieldsNotInCtor) {
      sb.append("  ")
          .append("result.")
          .append(fieldNotInCtor.name())
          .append(" = ")
          .append(fieldNotInCtor.name())
          .append(";")
          .append(System.lineSeparator());
    }
    sb.append("  ").append("return result;").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }
}
