package org.enso.runtime.parser.processor;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.util.SimpleElementVisitor14;
import org.enso.runtime.parser.dsl.IRChild;
import org.enso.runtime.parser.dsl.IRNode;

/**
 * Representation of an interface annotated with {@link org.enso.runtime.parser.dsl.IRNode}. Takes
 * care of - Methods from {@code org.enso.compiler.core.IR} with no default implementation. -
 * Methods annotated with {@link org.enso.runtime.parser.dsl.IRChild} (child elements). - Other
 * methods (will be fields of the record, not children).
 */
final class IRNodeElement {
  private final ProcessingEnvironment processingEnv;

  /** Name of the class that is being generated. */
  private final String className;

  /** User defined fields - all the abstract parameterless methods, including the inherited ones. */
  private final List<Field> fields;

  private static final String IMPORTS =
      """
import java.util.UUID;
import java.util.ArrayList;
import java.util.function.Function;
import org.enso.compiler.core.Identifier;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import scala.Option;
import scala.collection.immutable.List;
      """;

  /**
   * @param processingEnv
   * @param irNodeInterface Type element of the interface annotated with {@link IRNode}.
   * @param className Simple name (non-qualified) of the newly generated class.
   */
  IRNodeElement(
      ProcessingEnvironment processingEnv, TypeElement irNodeInterface, String className) {
    assert !className.contains(".") : "Class name should be simple, not qualified";
    this.processingEnv = processingEnv;
    this.className = className;
    this.fields = getAllFields(irNodeInterface);
  }

  /** Returns string representation of all necessary imports. */
  String imports() {
    var importsForFields =
        fields.stream()
            .map(field -> "import " + field.getQualifiedTypeName() + ";")
            .distinct()
            .collect(Collectors.joining(System.lineSeparator()));
    var allImports = IMPORTS + System.lineSeparator() + importsForFields;
    return allImports;
  }

  /**
   * Collects all abstract methods (with no parameters) from this interface and all the interfaces
   * that are extended by this interface. Every abstract method corresponds to a single field in the
   * newly generated record. Abstract methods annotated with {@link IRChild} are considered IR
   * children.
   *
   * @param irNodeInterface Type element of the interface annotated with {@link IRNode}.
   * @return List of fields
   */
  private List<Field> getAllFields(TypeElement irNodeInterface) {
    var fields = new ArrayList<Field>();

    var elemVisitor =
        new SimpleElementVisitor14<Void, Void>() {
          @Override
          protected Void defaultAction(Element e, Void unused) {
            for (var childElem : e.getEnclosedElements()) {
              childElem.accept(this, unused);
            }
            return null;
          }

          @Override
          public Void visitExecutable(ExecutableElement e, Void unused) {
            if (e.getParameters().isEmpty()) {
              var retType = e.getReturnType();
              var retTypeElem = (TypeElement) processingEnv.getTypeUtils().asElement(retType);
              var name = e.getSimpleName().toString();
              var childAnnot = e.getAnnotation(IRChild.class);
              boolean isChild = false;
              boolean isNullable = false;
              if (childAnnot != null) {
                ensureIsSubtypeOfIR(retTypeElem);
                isChild = true;
                isNullable = !childAnnot.required();
              }
              fields.add(new Field(retTypeElem, name, isNullable, isChild));
            }
            return super.visitExecutable(e, unused);
          }
        };
    irNodeInterface.accept(elemVisitor, null);
    return fields;
  }

  private void ensureIsSubtypeOfIR(TypeElement typeElem) {
    if (!Utils.isSubtypeOfIR(typeElem.asType(), processingEnv)) {
      Utils.printError(
          "Method annotated with @IRChild must return a subtype of IR interface",
          typeElem,
          processingEnv.getMessager());
    }
  }

  /**
   * Returns string representation of the class fields. Meant to be at the beginning of the class
   * body.
   */
  String fields() {
    var userDefinedFields =
        fields.stream()
            .map(field -> "private final " + field.getSimpleTypeName() + " " + field.getName())
            .collect(Collectors.joining(";" + System.lineSeparator()));
    var code =
        """
        $userDefinedFields;
        // Not final on purpose
        private DiagnosticStorage diagnostics;
        private MetadataStorage passData;
        private IdentifiedLocation location;
        private UUID id;
        """
            .replace("$userDefinedFields", userDefinedFields);
    return indent(code, 2);
  }

  /**
   * Returns string representation of the package-private constructor of the generated class. Note
   * that the constructor is meant to be invoked only by the internal Builder class.
   */
  String constructor() {
    var sb = new StringBuilder();
    sb.append("private ").append(className).append("(");
    var inParens =
        fields.stream()
            .map(
                field ->
                    "$fieldType $fieldName"
                        .replace("$fieldType", field.getSimpleTypeName())
                        .replace("$fieldName", field.getName()))
            .collect(Collectors.joining(", "));
    sb.append(inParens).append(") {").append(System.lineSeparator());
    var ctorBody =
        fields.stream()
            .map(field -> "  this.$fieldName = $fieldName;".replace("$fieldName", field.getName()))
            .collect(Collectors.joining(System.lineSeparator()));
    sb.append(indent(ctorBody, 2));
    sb.append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return indent(sb.toString(), 2);
  }

  private String childrenMethodBody() {
    var sb = new StringBuilder();
    var nl = System.lineSeparator();
    sb.append("var list = new ArrayList<IR>();").append(nl);
    fields.stream()
        .filter(Field::isChild)
        .forEach(
            childField -> {
              var childName = childField.getName();
              if (childField.isNullable()) {
                sb.append(
                    """
                if ($childName != null) {
                  list.add($childName);
                }
                """
                        .replace("$childName", childName));
              } else {
                sb.append(
                    """
                list.add($childName);
                """
                        .replace("$childName", childName));
              }
            });
    sb.append("return scala.jdk.javaapi.CollectionConverters.asScala(list).toList();").append(nl);
    return indent(sb.toString(), 2);
  }

  /**
   * Returns a String representing all the overriden methods from {@link org.enso.compiler.core.IR}.
   * Meant to be inside the generated record definition.
   */
  String overrideIRMethods() {
    var code =
        """

        @Override
        public MetadataStorage passData() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public Option<IdentifiedLocation> location() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR setLocation(Option<IdentifiedLocation> location) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR mapExpressions(Function<Expression, Expression> fn) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public List<IR> children() {
        $childrenMethodBody
        }

        @Override
        public @Identifier UUID getId() {
          if (id == null) {
            id = UUID.randomUUID();
          }
          return id;
        }

        @Override
        public DiagnosticStorage diagnostics() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public DiagnosticStorage getDiagnostics() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR duplicate(
          boolean keepLocations,
          boolean keepMetadata,
          boolean keepDiagnostics,
          boolean keepIdentifiers
        ) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public String showCode(int indent) {
          throw new UnsupportedOperationException("unimplemented");
        }
        """
            .replace("$childrenMethodBody", childrenMethodBody());
    return indent(code, 2);
  }

  /**
   * Returns string representation of all parameterless abstract methods from the interface
   * annotated with {@link IRNode}.
   *
   * @return Code of the overriden methods
   */
  String overrideUserDefinedMethods() {
    var code =
        fields.stream()
            .map(
                field ->
                    """
            @Override
            public $returnType $fieldName() {
              return $fieldName;
            }
            """
                        .replace("$returnType", field.getSimpleTypeName())
                        .replace("$fieldName", field.getName()))
            .collect(Collectors.joining(System.lineSeparator()));
    return indent(code, 2);
  }

  /**
   * Returns string representation of the code for the builder - that is a nested class that allows
   * to build the record.
   *
   * @return Code of the builder
   */
  String builder() {
    var fieldDeclarations =
        fields.stream()
            .map(
                field ->
                    """
            private $fieldType $fieldName;
            """
                        .replace("$fieldName", field.getName())
                        .replace("$fieldType", field.getSimpleTypeName()))
            .collect(Collectors.joining(System.lineSeparator()));

    var fieldSetters =
        fields.stream()
            .map(
                field ->
                    """
        public Builder $fieldName($fieldType $fieldName) {
          this.$fieldName = $fieldName;
          return this;
        }
        """
                        .replace("$fieldName", field.getName())
                        .replace("$fieldType", field.getSimpleTypeName()))
            .collect(Collectors.joining(System.lineSeparator()));

    // Validation code for all non-nullable fields
    var validationCode =
        fields.stream()
            .filter(field -> !field.isNullable())
            .map(
                field ->
                    """
            if (this.$fieldName == null) {
              throw new IllegalArgumentException("$fieldName is required");
            }
            """
                        .replace("$fieldName", field.getName()))
            .collect(Collectors.joining(System.lineSeparator()));

    var fieldList = fields.stream().map(Field::getName).collect(Collectors.joining(", "));

    var code =
        """
        public static final class Builder {
          $fieldDeclarations

          $fieldSetters

          public $className build() {
            validate();
            return new $className($fieldList);
          }

          private void validate() {
            $validationCode
          }
        }
        """
            .replace("$fieldDeclarations", fieldDeclarations)
            .replace("$fieldSetters", fieldSetters)
            .replace("$className", className)
            .replace("$fieldList", fieldList)
            .replace("$validationCode", validationCode);
    return indent(code, 2);
  }

  private static String indent(String code, int indentation) {
    return code.lines()
        .map(line -> " ".repeat(indentation) + line)
        .collect(Collectors.joining(System.lineSeparator()));
  }
}
