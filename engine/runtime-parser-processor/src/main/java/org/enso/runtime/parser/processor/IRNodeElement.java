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
  private final String recordName;
  private final List<Field> fields;

  private static final String IMPORTS =
      """
import java.util.UUID;
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
   * @param irNodeInterface
   * @param recordName Simple name (non-qualified) of the newly generated record.
   */
  IRNodeElement(
      ProcessingEnvironment processingEnv, TypeElement irNodeInterface, String recordName) {
    assert !recordName.contains(".") : "Record name should be simple, not qualified";
    this.processingEnv = processingEnv;
    this.recordName = recordName;
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
   * Returns string representation of record fields. Meant to be inside the generated record
   * definition.
   */
  String fields() {
    var userDefinedFields =
        fields.stream()
            .map(field -> field.getSimpleTypeName() + " " + field.getName())
            .collect(Collectors.joining(", " + System.lineSeparator()));
    var code =
        """
        $userDefinedFields,
        DiagnosticStorage diagnostics,
        MetadataStorage passData,
        IdentifiedLocation location
        """
            .replace("$userDefinedFields", userDefinedFields);
    return indent(code, 2);
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
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public @Identifier UUID getId() {
          throw new UnsupportedOperationException("unimplemented");
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
        """;
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
            $fieldType $fieldName;
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

          public $recordName build() {
            validate();
            // DiagnosticStorage, MetadataStorage, IdentifiedLocation are null initially.
            return new $recordName($fieldList, null, null, null);
          }

          private void validate() {
            $validationCode
          }
        }
        """
            .replace("$fieldDeclarations", fieldDeclarations)
            .replace("$fieldSetters", fieldSetters)
            .replace("$recordName", recordName)
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
