package org.enso.runtime.parser.processor.methodgen;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.IRProcessingException;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.utils.Utils;

/**
 * Code generator for {@code org.enso.compiler.core.ir.IR#duplicate} method or any of its override.
 * Note that in the interface hierarchy, there can be an override with a different return type.
 */
public class DuplicateMethodGenerator {

  private final GeneratedClassContext ctx;
  private final List<Parameter> parameters;

  /**
   * @param duplicateMethod ExecutableElement representing the duplicate method (or its override).
   */
  public DuplicateMethodGenerator(ExecutableElement duplicateMethod, GeneratedClassContext ctx) {
    this.ctx = Objects.requireNonNull(ctx);
    var boolType = ctx.getProcessingEnvironment().getTypeUtils().getPrimitiveType(TypeKind.BOOLEAN);
    this.parameters =
        List.of(
            new Parameter(boolType, "keepLocations"),
            new Parameter(boolType, "keepMetadata"),
            new Parameter(boolType, "keepDiagnostics"),
            new Parameter(boolType, "keepIdentifiers"));
    ensureDuplicateMethodHasExpectedSignature(duplicateMethod);
  }

  private void ensureDuplicateMethodHasExpectedSignature(ExecutableElement duplicateMethod) {
    var dupMethodParameters = duplicateMethod.getParameters();
    if (dupMethodParameters.size() != parameters.size()) {
      throw new IRProcessingException(
          "Duplicate method must have " + parameters.size() + " parameters", duplicateMethod);
    }
    var allParamsAreBooleans =
        dupMethodParameters.stream().allMatch(par -> par.asType().getKind() == TypeKind.BOOLEAN);
    if (!allParamsAreBooleans) {
      throw new IRProcessingException(
          "All parameters of the duplicate method must be of type boolean", duplicateMethod);
    }
  }

  /**
   * Generate code for two duplicate methods - one overridden with all four parameters, and another
   * parameterless that just delegates to the first one.
   */
  public String generateDuplicateMethodsCode() {
    var sb = new StringBuilder();
    sb.append("@Override").append(System.lineSeparator());
    sb.append("public ")
        .append(dupMethodRetType())
        .append(" duplicate(")
        .append(parameters.stream().map(Parameter::toString).collect(Collectors.joining(", ")))
        .append(") {")
        .append(System.lineSeparator());
    var duplicatedVars = new ArrayList<DuplicateVar>();

    var duplicateMetaFieldsCode =
        """
        $diagType diagnosticsDuplicated = null;
        if (keepDiagnostics && this.diagnostics != null) {
          diagnosticsDuplicated = this.diagnostics.copy();
        }
        $metaType passDataDuplicated = null;
        if (keepMetadata && this.passData != null) {
          passDataDuplicated = this.passData.duplicate();
        }
        $locType locationDuplicated = null;
        if (keepLocations && this.location != null) {
          locationDuplicated = this.location;
        }
        $idType idDuplicated = null;
        if (keepIdentifiers && this.id != null) {
          idDuplicated = this.id;
        }
        """
            .replace("$locType", ctx.getLocationMetaField().getSimpleTypeName())
            .replace("$metaType", ctx.getPassDataMetaField().getSimpleTypeName())
            .replace("$diagType", ctx.getDiagnosticsMetaField().getSimpleTypeName())
            .replace("$idType", ctx.getIdMetaField().getSimpleTypeName());
    sb.append(Utils.indent(duplicateMetaFieldsCode, 2));
    sb.append(System.lineSeparator());
    for (var metaVar : metaFields()) {
      var dupName = metaVar.name + "Duplicated";
      duplicatedVars.add(new DuplicateVar(metaVar.type, dupName, metaVar.name, false));
    }

    for (var field : ctx.getUserFields()) {
      if (field.isChild()) {
        if (field.isNullable()) {
          sb.append(Utils.indent(nullableChildCode(field), 2));
          sb.append(System.lineSeparator());
          duplicatedVars.add(
              new DuplicateVar(field.getType(), dupFieldName(field), field.getName(), true));
        } else {
          if (field.isList()) {
            sb.append(Utils.indent(listChildCode(field), 2));
            sb.append(System.lineSeparator());
            duplicatedVars.add(
                new DuplicateVar(field.getType(), dupFieldName(field), field.getName(), false));
          } else if (field.isOption()) {
            sb.append(Utils.indent(optionChildCode(field), 2));
            sb.append(System.lineSeparator());
            duplicatedVars.add(
                new DuplicateVar(field.getType(), dupFieldName(field), field.getName(), false));
          } else {
            sb.append(Utils.indent(notNullableChildCode(field), 2));
            sb.append(System.lineSeparator());
            duplicatedVars.add(
                new DuplicateVar(field.getType(), dupFieldName(field), field.getName(), true));
          }
        }
      } else {
        sb.append(Utils.indent(nonChildCode(field), 2));
        sb.append(System.lineSeparator());
        duplicatedVars.add(
            new DuplicateVar(field.getType(), dupFieldName(field), field.getName(), false));
      }
    }

    var ctorParams = matchCtorParams(duplicatedVars);
    var newSubclass = newSubclass(ctorParams);
    sb.append(newSubclass);

    // Rest of the fields that need to be set
    var restOfDuplicatedVars = Utils.diff(duplicatedVars, ctorParams);
    for (var duplVar : restOfDuplicatedVars) {
      sb.append("  ").append("duplicated.").append(duplVar.originalName).append(" = ");
      if (duplVar.needsCast) {
        sb.append("(").append(duplVar.type).append(") ");
      }
      sb.append(duplVar.duplicatedName).append(";").append(System.lineSeparator());
    }

    sb.append("  ").append("return duplicated;").append(System.lineSeparator());

    sb.append("}");
    sb.append(System.lineSeparator());
    var defaultDuplicateMethod = sb.toString();
    return defaultDuplicateMethod + System.lineSeparator() + parameterlessDuplicateMethod();
  }

  private List<MetaField> metaFields() {
    var procEnv = ctx.getProcessingEnvironment();
    var diagTypeElem = Utils.diagnosticStorageTypeElement(procEnv);
    var metaTypeElem = Utils.metadataStorageTypeElement(procEnv);
    var locationTypeElem = Utils.identifiedLocationTypeElement(procEnv);
    var uuidTypeElem = Utils.uuidTypeElement(procEnv);
    return List.of(
        new MetaField(diagTypeElem.asType(), "diagnostics"),
        new MetaField(metaTypeElem.asType(), "passData"),
        new MetaField(locationTypeElem.asType(), "location"),
        new MetaField(uuidTypeElem.asType(), "id"));
  }

  private String parameterlessDuplicateMethod() {
    var code =
        """
        public $retType duplicate() {
          return duplicate(true, true, true, false);
        }
        """
            .replace("$retType", dupMethodRetType());
    return code;
  }

  private static String dupFieldName(Field field) {
    return field.getName() + "Duplicated";
  }

  private String nullableChildCode(Field nullableChild) {
    Utils.hardAssert(nullableChild.isNullable() && nullableChild.isChild());
    return """
          IR $dupName = null;
            if ($childName != null) {
              $dupName = $childName.duplicate($parameterNames);
              if (!($dupName instanceof $childType)) {
                throw new IllegalStateException("Duplicated child is not of the expected type: " + $dupName);
              }
          }
        """
        .replace("$childType", nullableChild.getSimpleTypeName())
        .replace("$childName", nullableChild.getName())
        .replace("$dupName", dupFieldName(nullableChild))
        .replace("$parameterNames", String.join(", ", parameterNames()));
  }

  private String notNullableChildCode(Field child) {
    assert child.isChild() && !child.isNullable() && !child.isList() && !child.isOption();
    return """
          IR $dupName = $childName.duplicate($parameterNames);
          if (!($dupName instanceof $childType)) {
            throw new IllegalStateException("Duplicated child is not of the expected type: " + $dupName);
          }
          """
        .replace("$childType", child.getSimpleTypeName())
        .replace("$childName", child.getName())
        .replace("$dupName", dupFieldName(child))
        .replace("$parameterNames", String.join(", ", parameterNames()));
  }

  private String listChildCode(Field listChild) {
    Utils.hardAssert(listChild.isChild() && listChild.isList());
    return """
          $childListType $dupName =
            $childName.map(child -> {
              IR dupChild = child.duplicate($parameterNames);
              if (!(dupChild instanceof $childType)) {
                throw new IllegalStateException("Duplicated child is not of the expected type: " + dupChild);
              }
              return ($childType) dupChild;
            });
          """
        .replace("$childListType", listChild.getSimpleTypeName())
        .replace("$childType", listChild.getTypeParameter().getSimpleName())
        .replace("$childName", listChild.getName())
        .replace("$dupName", dupFieldName(listChild))
        .replace("$parameterNames", String.join(", ", parameterNames()));
  }

  private String optionChildCode(Field optionChild) {
    Utils.hardAssert(optionChild.isOption() && optionChild.isChild());
    return """
        $childOptType $dupName = $childName;
        if ($childName.isDefined()) {
          var duplicated = $childName.get().duplicate($parameterNames);
          if (!(duplicated instanceof $childType)) {
            throw new IllegalStateException("Duplicated child is not of the expected type: " + $dupName);
          }
          $dupName = Option.apply(duplicated);
        }
        """
        .replace("$childOptType", optionChild.getSimpleTypeName())
        .replace("$childType", optionChild.getTypeParameter().getSimpleName())
        .replace("$childName", optionChild.getName())
        .replace("$dupName", dupFieldName(optionChild))
        .replace("$parameterNames", String.join(", ", parameterNames()));
  }

  private static String nonChildCode(Field field) {
    Utils.hardAssert(!field.isChild());
    return """
        $childType $dupName = $childName;
        """
        .replace("$childType", field.getSimpleTypeName())
        .replace("$childName", field.getName())
        .replace("$dupName", dupFieldName(field));
  }

  private List<String> parameterNames() {
    return parameters.stream().map(Parameter::name).collect(Collectors.toList());
  }

  /** Generate code for call of a constructor of the subclass. */
  private String newSubclass(List<DuplicateVar> ctorParams) {
    var subClassType = ctx.getProcessedClass().getClazz().getSimpleName().toString();
    var ctor = ctx.getProcessedClass().getCtor();
    Utils.hardAssert(ctor.getParameters().size() == ctorParams.size());
    var sb = new StringBuilder();
    sb.append("  ")
        .append(subClassType)
        .append(" duplicated")
        .append(" = ")
        .append("new ")
        .append(subClassType)
        .append("(");
    var ctorParamsStr =
        ctorParams.stream()
            .map(
                ctorParam -> {
                  if (ctorParam.needsCast) {
                    return "(" + ctorParam.type + ") " + ctorParam.duplicatedName;
                  } else {
                    return ctorParam.duplicatedName;
                  }
                })
            .collect(Collectors.joining(", "));
    sb.append(ctorParamsStr).append(");").append(System.lineSeparator());
    return sb.toString();
  }

  /**
   * Returns sublist of the given list that matches the parameters of the constructor of the
   * subclass.
   *
   * @param duplicatedVars All duplicated variables.
   * @return sublist, potentially reordered.
   */
  private List<DuplicateVar> matchCtorParams(List<DuplicateVar> duplicatedVars) {
    var ctorParams = new ArrayList<DuplicateVar>();
    for (var subclassCtorParam : ctx.getSubclassConstructorParameters()) {
      var paramType = subclassCtorParam.getTypeName();
      var paramName = subclassCtorParam.name();
      duplicatedVars.stream()
          .filter(
              var ->
                  var.type.equals(subclassCtorParam.getType())
                      && var.originalName.equals(paramName))
          .findFirst()
          .ifPresentOrElse(
              ctorParams::add,
              () -> {
                var errMsg =
                    String.format(
                        "No matching field found for parameter %s of type %s. All duplicated vars:"
                            + " %s",
                        paramName, paramType, duplicatedVars);
                throw new IRProcessingException(errMsg, ctx.getProcessedClass().getCtor());
              });
    }
    return ctorParams;
  }

  private String dupMethodRetType() {
    return ctx.getProcessedClass().getClazz().getSimpleName().toString();
  }

  /**
   * @param duplicatedName Name of the duplicated variable
   * @param originalName Name of the original variable (field)
   * @param needsCast If the duplicated variable needs to be cast to its type in the return
   *     statement.
   */
  private record DuplicateVar(
      TypeMirror type, String duplicatedName, String originalName, boolean needsCast) {}

  /**
   * Parameter for the duplicate method
   *
   * @param type
   * @param name
   */
  private record Parameter(TypeMirror type, String name) {

    @Override
    public String toString() {
      return type + " " + name;
    }
  }

  private record MetaField(TypeMirror type, String name) {}
}
