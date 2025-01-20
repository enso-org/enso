package org.enso.runtime.parser.processor;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.processing.ProcessingEnvironment;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.field.FieldCollector;
import org.enso.runtime.parser.processor.methodgen.BuilderMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.CopyMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.DuplicateMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.EqualsMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.HashCodeMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.MapExpressionsMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.SetLocationMethodGenerator;
import org.enso.runtime.parser.processor.methodgen.ToStringMethodGenerator;
import org.enso.runtime.parser.processor.utils.Utils;

/**
 * Generates code for a super class for a class annotated with {@link
 * org.enso.runtime.parser.dsl.GenerateIR}.
 */
final class IRNodeClassGenerator {
  private final ProcessingEnvironment processingEnv;
  private final ProcessedClass processedClass;

  /** Name of the class that is being generated */
  private final String className;

  private final GeneratedClassContext generatedClassContext;
  private final DuplicateMethodGenerator duplicateMethodGenerator;
  private final CopyMethodGenerator copyMethodGenerator;
  private final SetLocationMethodGenerator setLocationMethodGenerator;
  private final BuilderMethodGenerator builderMethodGenerator;
  private final MapExpressionsMethodGenerator mapExpressionsMethodGenerator;
  private final EqualsMethodGenerator equalsMethodGenerator;
  private final HashCodeMethodGenerator hashCodeMethodGenerator;
  private final ToStringMethodGenerator toStringMethodGenerator;

  private static final Set<String> defaultImportedTypes =
      Set.of(
          "java.util.UUID",
          "java.util.ArrayList",
          "java.util.function.Function",
          "java.util.Objects",
          "java.util.stream.Collectors",
          "org.enso.compiler.core.Identifier",
          "org.enso.compiler.core.IR",
          "org.enso.compiler.core.ir.DiagnosticStorage",
          "org.enso.compiler.core.ir.DiagnosticStorage$",
          "org.enso.compiler.core.ir.Expression",
          "org.enso.compiler.core.ir.IdentifiedLocation",
          "org.enso.compiler.core.ir.MetadataStorage",
          "scala.Option");

  /**
   * @param className Name of the generated class. Non qualified.
   */
  IRNodeClassGenerator(
      ProcessingEnvironment processingEnv, ProcessedClass processedClass, String className) {
    Utils.hardAssert(!className.contains("."), "Class name should be simple, not qualified");
    this.processingEnv = processingEnv;
    this.processedClass = processedClass;
    this.className = className;
    var userFields = getAllUserFields(processedClass);
    var duplicateMethod =
        Utils.findDuplicateMethod(processedClass.getIrInterfaceElem(), processingEnv);
    this.generatedClassContext =
        new GeneratedClassContext(className, userFields, processingEnv, processedClass);
    this.duplicateMethodGenerator =
        new DuplicateMethodGenerator(duplicateMethod, generatedClassContext);
    this.copyMethodGenerator = new CopyMethodGenerator(generatedClassContext);
    this.builderMethodGenerator = new BuilderMethodGenerator(generatedClassContext);
    var mapExpressionsMethod =
        Utils.findMapExpressionsMethod(processedClass.getIrInterfaceElem(), processingEnv);
    this.mapExpressionsMethodGenerator =
        new MapExpressionsMethodGenerator(mapExpressionsMethod, generatedClassContext);
    var setLocationMethod =
        Utils.findMethod(
            processedClass.getIrInterfaceElem(),
            processingEnv,
            method -> method.getSimpleName().toString().equals("setLocation"));
    this.setLocationMethodGenerator =
        new SetLocationMethodGenerator(setLocationMethod, generatedClassContext);
    this.equalsMethodGenerator = new EqualsMethodGenerator(generatedClassContext);
    this.hashCodeMethodGenerator = new HashCodeMethodGenerator(generatedClassContext);
    this.toStringMethodGenerator = new ToStringMethodGenerator(generatedClassContext);
  }

  /** Returns simple name of the generated class. */
  String getClassName() {
    return className;
  }

  /** Returns set of import statements that should be included in the generated class. */
  Set<String> imports() {
    var importsForFields =
        generatedClassContext.getUserFields().stream()
            .flatMap(field -> field.getImportedTypes().stream())
            .collect(Collectors.toUnmodifiableSet());
    var allImports = new HashSet<String>();
    allImports.addAll(defaultImportedTypes);
    allImports.addAll(importsForFields);
    return allImports.stream()
        .map(importedType -> "import " + importedType + ";")
        .collect(Collectors.toUnmodifiableSet());
  }

  /** Generates the body of the class - fields, field setters, method overrides, builder, etc. */
  String classBody() {
    var code =
        """
        $fields

        $defaultCtor

        $validateConstructor

        public static Builder builder() {
          return new Builder();
        }

        $copyMethod

        $userDefinedGetters

        $overrideIRMethods

        $mapExpressionsMethod

        $equalsMethod

        $hashCodeMethod

        $toStringMethod

        $builder
        """
            .replace("$fields", fieldsCode())
            .replace("$defaultCtor", defaultConstructor())
            .replace("$validateConstructor", validateConstructor())
            .replace("$copyMethod", copyMethodGenerator.generateMethodCode())
            .replace("$userDefinedGetters", userDefinedGetters())
            .replace("$overrideIRMethods", overrideIRMethods())
            .replace("$mapExpressionsMethod", mapExpressions())
            .replace("$equalsMethod", equalsMethodGenerator.generateMethodCode())
            .replace("$hashCodeMethod", hashCodeMethodGenerator.generateMethodCode())
            .replace("$toStringMethod", toStringMethodGenerator.generateMethodCode())
            .replace("$builder", builderMethodGenerator.generateBuilder());
    return Utils.indent(code, 2);
  }

  private List<Field> getAllUserFields(ProcessedClass processedClass) {
    var fieldCollector = new FieldCollector(processingEnv, processedClass);
    return fieldCollector.collectFields();
  }

  /**
   * Returns string representation of the class fields. Meant to be at the beginning of the class
   * body.
   */
  private String fieldsCode() {
    var userDefinedFields =
        generatedClassContext.getUserFields().stream()
            .map(field -> "private final " + field.getSimpleTypeName() + " " + field.getName())
            .collect(Collectors.joining(";" + System.lineSeparator()));
    var code =
        """
        $userDefinedFields;
        // The following meta fields cannot be private, as we are explicitly
        // setting them in the `duplicate` method. Inheritor should not access
        // these fields directly
        protected DiagnosticStorage diagnostics;
        protected MetadataStorage passData;
        protected IdentifiedLocation location;
        protected UUID id;
        """
            .replace("$userDefinedFields", userDefinedFields);
    return code;
  }

  /**
   * Returns string representation of the protected constructor of the generated class. The default
   * constructor has parameters for both meta fields and user-defined fields.
   */
  private String defaultConstructor() {
    var docs =
        """
        /**
         * Default constructor matching the signature of subtype's constructor.
         * The rest of the fields not specified as parameters to this constructor are initialized to
         * their default value.
         */
        """;
    var subclassCtorParams = generatedClassContext.getSubclassConstructorParameters();
    var allFields = generatedClassContext.getAllFields();
    var diff = Utils.diff(allFields, subclassCtorParams);
    var ctorCode = constructorForFields(subclassCtorParams, diff);
    return docs + ctorCode;
  }

  /**
   * The caller must ensure that parameters and {@code initializeToNull} are disjoint sets and that
   * the union of them is equal to the set of all fields in the generated class.
   *
   * @param parameters Fields that will be parameters of the constructor. Can be empty list.
   * @param initializeToNull Rest of the fields that will be initialized to null in the constructor.
   *     Can be empty list.
   */
  private String constructorForFields(
      List<ClassField> parameters, List<ClassField> initializeToNull) {
    Utils.hardAssert(
        !(parameters.isEmpty() && initializeToNull.isEmpty()),
        "At least one of the list must be non empty");
    var sb = new StringBuilder();
    sb.append("protected ").append(className).append("(");
    var inParens =
        parameters.stream()
            .map(
                consParam ->
                    "$consType $consName"
                        .replace("$consType", consParam.getSimpleTypeName())
                        .replace("$consName", consParam.name()))
            .collect(Collectors.joining(", "));
    sb.append(inParens).append(") {").append(System.lineSeparator());

    if (!parameters.isEmpty()) {
      var ctorBody =
          parameters.stream()
              .map(field -> "  this.$fieldName = $fieldName;".replace("$fieldName", field.name()))
              .collect(Collectors.joining(System.lineSeparator()));
      sb.append(ctorBody);
    }
    sb.append(System.lineSeparator());

    // The rest of the constructor body initializes the rest of the fields to null.
    if (!initializeToNull.isEmpty()) {
      var initToNullBody =
          initializeToNull.stream()
              .map(
                  field -> {
                    var initializer = field.initializer() != null ? field.initializer() : "null";
                    return "  this.$fieldName = $init;"
                        .replace("$fieldName", field.name())
                        .replace("$init", initializer);
                  })
              .collect(Collectors.joining(System.lineSeparator()));
      sb.append(initToNullBody);
      sb.append(System.lineSeparator());
    }
    sb.append("  validateConstructor();").append(System.lineSeparator());
    sb.append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * Generates code for validation at the end of the constructor. Validates if all the required
   * fields were set in the constructor (passed as params).
   */
  private String validateConstructor() {
    var sb = new StringBuilder();
    sb.append(
        """
        /**
         * Validates if all the required fields were set in the constructor.
         */
        """);
    sb.append("private void validateConstructor() {").append(System.lineSeparator());
    var checkCode =
        generatedClassContext.getAllFields().stream()
            .filter(field -> !field.canBeNull())
            .filter(field -> !field.isPrimitive())
            .map(
                notNullField ->
                    """
            if ($fieldName == null) {
              throw new IllegalArgumentException("$fieldName is required");
            }
            """
                        .replace("$fieldName", notNullField.name()))
            .collect(Collectors.joining(System.lineSeparator()));
    sb.append(Utils.indent(checkCode, 2));
    sb.append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  private String childrenMethodBody() {
    var sb = new StringBuilder();
    var nl = System.lineSeparator();
    sb.append("var list = new ArrayList<IR>();").append(nl);
    generatedClassContext.getUserFields().stream()
        .filter(Field::isChild)
        .forEach(
            childField -> {
              String addToListCode;
              if (childField.isList()) {
                addToListCode =
                    """
                    $childName.foreach(list::add);
                    """
                        .replace("$childName", childField.getName());
              } else if (childField.isOption()) {
                addToListCode =
                    """
                    if ($childName.isDefined()) {
                      list.add($childName.get());
                    }
                    """
                        .replace("$childName", childField.getName());
              } else {
                addToListCode = "list.add(" + childField.getName() + ");";
              }

              var childName = childField.getName();
              if (childField.isNullable()) {
                sb.append(
                    """
                if ($childName != null) {
                  $addToListCode
                }
                """
                        .replace("$childName", childName)
                        .replace("$addToListCode", addToListCode));
              } else {
                sb.append(addToListCode).append(nl);
              }
            });
    sb.append("return scala.jdk.javaapi.CollectionConverters.asScala(list).toList();").append(nl);
    return indent(sb.toString(), 2);
  }

  /**
   * Returns a String representing all the overriden methods from {@code org.enso.compiler.core.IR}.
   * Meant to be inside the generated record definition.
   */
  private String overrideIRMethods() {
    var code =
        """

        @Override
        public MetadataStorage passData() {
          assert passData != null : "passData must always be initialized";
          return passData;
        }

        @Override
        public Option<IdentifiedLocation> location() {
          if (location == null) {
            return scala.Option.empty();
          } else {
            return scala.Option.apply(location);
          }
        }

        $setLocationMethod

        @Override
        public IdentifiedLocation identifiedLocation() {
          return this.location;
        }

        @Override
        public scala.collection.immutable.List<IR> children() {
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
          return diagnostics;
        }

        @Override
        public DiagnosticStorage getDiagnostics() {
          if (diagnostics == null) {
            diagnostics = DiagnosticStorage$.MODULE$.createEmpty();
          }
          return diagnostics;
        }

        public DiagnosticStorage diagnosticsCopy() {
          if (diagnostics == null) {
            return null;
          } else {
            return diagnostics.copy();
          }
        }

        $duplicateMethods

        @Override
        public String showCode(int indent) {
          throw new UnsupportedOperationException("unimplemented");
        }
        """
            .replace("$childrenMethodBody", childrenMethodBody())
            .replace("$setLocationMethod", setLocationMethodGenerator.generateMethodCode())
            .replace("$duplicateMethods", duplicateMethodGenerator.generateDuplicateMethodsCode());
    return code;
  }

  /** Returns string representation of all getters for the user-defined fields. */
  private String userDefinedGetters() {
    var code =
        generatedClassContext.getUserFields().stream()
            .map(
                field ->
                    """
            public $returnType $fieldName() {
              return $fieldName;
            }
            """
                        .replace("$returnType", field.getSimpleTypeName())
                        .replace("$fieldName", field.getName()))
            .collect(Collectors.joining(System.lineSeparator()));
    return code;
  }

  private String mapExpressions() {
    return mapExpressionsMethodGenerator.generateMapExpressionsMethodCode();
  }

  private static String indent(String code, int indentation) {
    return code.lines()
        .map(line -> " ".repeat(indentation) + line)
        .collect(Collectors.joining(System.lineSeparator()));
  }
}
