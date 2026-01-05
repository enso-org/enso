package org.enso.runtime.parser.processor.methodgen;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.lang.model.element.ExecutableElement;
import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.IRProcessingException;
import org.enso.runtime.parser.processor.field.Field;
import org.enso.runtime.parser.processor.field.ListField;
import org.enso.runtime.parser.processor.field.OptionField;
import org.enso.runtime.parser.processor.field.OptionListField;
import org.enso.runtime.parser.processor.field.PersistanceReferenceField;
import org.enso.runtime.parser.processor.utils.Utils;

public final class MapExpressionsMethodGenerator {
  private final ExecutableElement mapExpressionsMethod;
  private final GeneratedClassContext ctx;
  private static final String METHOD_NAME = "mapExpressions";

  private static final String DEF_ARG_CLASS =
      "org.enso.compiler.core.ir.DefinitionArgument.Specified";
  private static final String CALL_ARG_CLASS = "org.enso.compiler.core.ir.CallArgument.Specified";
  private static final String DEF_TYPE_CLASS =
      "org.enso.compiler.core.ir.module.scope.Definition.Type";
  private static final String DEF_DATA_CLASS =
      "org.enso.compiler.core.ir.module.scope.Definition.Data";
  private static final String PATTERN_NAME_CLASS = "org.enso.compiler.core.ir.Pattern.Name";
  private static final String PATTERN_TYPE_CLASS = "org.enso.compiler.core.ir.Pattern.Type";
  private static final String PATTERN_CONSTRUCTOR_CLASS =
      "org.enso.compiler.core.ir.Pattern.Constructor";

  /**
   * @param mapExpressionsMethod Reference to {@code mapExpressions} method in the interface for
   *     which the class is generated.
   * @param ctx
   */
  public MapExpressionsMethodGenerator(
      ExecutableElement mapExpressionsMethod, GeneratedClassContext ctx) {
    ensureMapExpressionsMethodHasExpectedSignature(mapExpressionsMethod);
    this.mapExpressionsMethod = mapExpressionsMethod;
    this.ctx = Objects.requireNonNull(ctx);
  }

  private void ensureMapExpressionsMethodHasExpectedSignature(
      ExecutableElement mapExpressionsMethod) {
    var parameters = mapExpressionsMethod.getParameters();
    if (parameters.size() != 1) {
      throw new IRProcessingException(
          "Map expressions method must have 1 parameter", mapExpressionsMethod);
    }
  }

  public String generateMapExpressionsMethodCode() {
    var sb = new StringBuilder();
    var subclassType = ctx.getProcessedClassName();
    sb.append(doMapExprCode());
    sb.append(System.lineSeparator());
    sb.append(System.lineSeparator());

    sb.append("@Override").append(System.lineSeparator());
    sb.append("public ")
        .append(subclassType)
        .append(" ")
        .append(METHOD_NAME)
        .append("(")
        .append(
            "java.util.function.Function<org.enso.compiler.core.ir.Expression,"
                + " org.enso.compiler.core.ir.Expression> fn")
        .append(") {")
        .append(System.lineSeparator());

    var children = ctx.getUserFields().stream().filter(Field::isChild);
    // A list of new children that are created by calling mapExpressions on the existing children
    // Or the function directly if the child is of Expression type (this prevents
    // recursion).
    var newChildren =
        children
            .map(
                child -> {
                  var childTypeParameter = child.getTypeParameter();
                  if (child instanceof OptionListField optionListField) {
                    childTypeParameter = optionListField.getNestedTypeParameter();
                  }
                  ExecutableElement childsMapExprMethod;
                  if (childTypeParameter != null) {
                    childsMapExprMethod =
                        Utils.findMapExpressionsMethod(
                            childTypeParameter, ctx.getProcessingEnvironment());
                  } else {
                    var childTypeElem = Utils.typeMirrorToElement(child.getType());
                    childsMapExprMethod =
                        Utils.findMapExpressionsMethod(
                            childTypeElem, ctx.getProcessingEnvironment());
                  }

                  var typeUtils = ctx.getProcessingEnvironment().getTypeUtils();
                  var childsMapExprMethodRetType =
                      typeUtils.asElement(childsMapExprMethod.getReturnType());
                  var shouldCast =
                      !typeUtils.isSameType(child.getType(), childsMapExprMethodRetType.asType());
                  if (child.isList() || child.isOption()) {
                    shouldCast = false;
                  }
                  if (child.isPersistanceReference()) {
                    assert childTypeParameter != null;
                    shouldCast =
                        !typeUtils.isSameType(
                            childTypeParameter.asType(), childsMapExprMethodRetType.asType());
                  }
                  var newChildType = Utils.qualifiedTypeName(childsMapExprMethodRetType);

                  var newChildName = child.getName() + "Mapped";
                  var mapCode =
                      switch (child) {
                        case PersistanceReferenceField perRefField ->
                            mapPersistanceReference(newChildName, perRefField);
                        case ListField listField -> mapList(newChildName, listField);
                        case OptionField optionField -> mapOption(newChildName, optionField);
                        case OptionListField optionListField ->
                            mapOptionListField(newChildName, optionListField);
                        default -> mapOther(newChildName, newChildType, child);
                      };
                  var startComment =
                      """
                      //  === Start of mapping code for ${fieldName} ===
                      """
                          .replace("${fieldName}", child.getName());
                  var endComment =
                      """
                      //  === End of mapping code for ${fieldName} ===
                      """
                          .replace("${fieldName}", child.getName());
                  sb.append(Utils.indent(startComment, 2));
                  sb.append(System.lineSeparator());
                  sb.append(Utils.indent(mapCode, 2));
                  sb.append(System.lineSeparator());
                  sb.append(Utils.indent(endComment, 2));
                  sb.append(System.lineSeparator());

                  return new MappedChild(newChildName, child, shouldCast);
                })
            .toList();
    if (newChildren.isEmpty()) {
      sb.append("  return ")
          .append("(")
          .append(ctx.getProcessedClassName())
          .append(") this;")
          .append(System.lineSeparator());
      sb.append("}").append(System.lineSeparator());
      return sb.toString();
    }
    sb.append("  // Only copy if some of the children actually changed")
        .append(System.lineSeparator());
    var changedCond =
        newChildren.stream()
            .map(
                newChild ->
                    "(!Objects.equals(${mappedChildName}, ${childName}))"
                        .replace("${mappedChildName}", newChild.newChildName)
                        .replace("${childName}", newChild.child.getName()))
            .collect(Collectors.joining(" || "));
    sb.append("  ").append("if (").append(changedCond).append(") {").append(System.lineSeparator());
    sb.append("    ").append("var bldr = new Builder();").append(System.lineSeparator());
    for (MappedChild newChild : newChildren) {
      if (newChild.shouldCast) {
        sb.append("    ")
            .append("if (!(")
            .append(newChild.newChildName)
            .append(" instanceof ")
            .append(newChild.child.getQualifiedTypeName())
            .append(")) {")
            .append(System.lineSeparator());
        sb.append("      ")
            .append(
                "throw new IllegalStateException(\"Duplicated child is not of the expected"
                    + " type: \" + ")
            .append(newChild.newChildName)
            .append(");")
            .append(System.lineSeparator());
        sb.append("    }").append(System.lineSeparator());
      }
      sb.append("    ").append("bldr.").append(newChild.child.getName()).append("(");
      if (newChild.shouldCast) {
        sb.append("(").append(newChild.child.getQualifiedTypeName()).append(") ");
      }
      sb.append(newChild.newChildName).append(");").append(System.lineSeparator());
    }
    for (var field : restOfUserFields(newChildren)) {
      sb.append("    ")
          .append("bldr.")
          .append(field.getName())
          .append("(")
          .append(field.getName())
          .append(");")
          .append(System.lineSeparator());
    }
    // Meta fields are handled specifically - some of them need to be duplicated,
    // some of them does not.
    // Note: Keep the indentation of the multiline string.
    sb.append(
        """
            if (this.diagnostics != null) {
              bldr.diagnostics(this.diagnostics.copy());
            }
            if (this.passData != null) {
              // passData should not be duplicated, i.e., no call of `this.passData.duplicate()`
              // method. Just assign the same reference.
              bldr.passData(this.passData);
            }
            if (this.location != null) {
              bldr.location(this.location);
            }
            if (this.id != null) {
              bldr.id(this.id);
            }
        """);
    sb.append("    return bldr.build();").append(System.lineSeparator());
    sb.append("  } else { ").append(System.lineSeparator());
    sb.append("    // None of the mapped children changed - just return this")
        .append(System.lineSeparator());
    sb.append("    return ")
        .append("(")
        .append(ctx.getProcessedClassName())
        .append(") this;")
        .append(System.lineSeparator());
    sb.append("  }").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  private boolean isProcessingDefinitionArgument() {
    return ctx.getProcessedClass().getClazz().getQualifiedName().toString().equals(DEF_ARG_CLASS);
  }

  private boolean isProcessingCallArgument() {
    return ctx.getProcessedClass().getClazz().getQualifiedName().toString().equals(CALL_ARG_CLASS);
  }

  private boolean isProcessingDefinitionType() {
    return ctx.getProcessedClass().getClazz().getQualifiedName().toString().equals(DEF_TYPE_CLASS);
  }

  private boolean isProcessingDefinitionData() {
    return ctx.getProcessedClass().getClazz().getQualifiedName().toString().equals(DEF_DATA_CLASS);
  }

  private boolean isProcessingPatternName() {
    return ctx.getProcessedClassName().equals(PATTERN_NAME_CLASS);
  }

  private boolean isProcessingPatternType() {
    return ctx.getProcessedClassName().equals(PATTERN_TYPE_CLASS);
  }

  private boolean isProcessingPatternConstructor() {
    return ctx.getProcessedClassName().equals(PATTERN_CONSTRUCTOR_CLASS);
  }

  private String doMapExprCode() {
    var specialHandling = new StringBuilder();
    if (isProcessingDefinitionArgument()) {
      specialHandling.append(
          """
            // Special case - name of DefinitionArgument is not applied.
            // This means no `fn.apply` call on it.
            assert this instanceof ${defArgClass};
            if (ir == this.name()) {
              return (T) ir.mapExpressions(fn);
            }
          """
              .replace("${defArgClass}", DEF_ARG_CLASS));
    }
    if (isProcessingCallArgument()) {
      specialHandling.append(
          """
            // Special case - name of CallArgument is not applied.
            // This means no `fn.apply` call on it.
            assert this instanceof ${callArgClass};
            if (this.name().isDefined()
                && ir == this.name().get()) {
              return (T) ir.mapExpressions(fn);
            }
          """
              .replace("${callArgClass}", CALL_ARG_CLASS));
    }
    if (isProcessingDefinitionType()) {
      specialHandling.append(
          """
            // Special case - name of Definition.Type is ignored.
            assert this instanceof ${defTypeClass};
            if (ir == this.name()) {
              return ir;
            }
          """
              .replace("${defTypeClass}", DEF_TYPE_CLASS));
    }
    if (isProcessingDefinitionData()) {
      specialHandling.append(
          """
            // Special case - name of Definition.Data is not applied.
            // This means no `fn.apply` call on it.
            assert this instanceof ${defDataClass};
            if (ir == this.name()) {
              return (T) ir.mapExpressions(fn);
            }
          """
              .replace("${defDataClass}", DEF_DATA_CLASS));
    }
    if (isProcessingPatternName()) {
      specialHandling.append(
          """
          // Special case - name of `Pattern.Name` is not processed.
          // This means no `fn.apply` call on it.
          assert this instanceof ${patternNameClass};
          if (ir == this.name()) {
            return (T) ir.mapExpressions(fn);
          }
          """
              .replace("${patternNameClass}", PATTERN_NAME_CLASS));
    }
    if (isProcessingPatternType()) {
      specialHandling.append(
          """
          // Special case - `name` and `tpe` of `Pattern.Type` are not processed.
          // This means no `fn.apply` call on them.
          assert this instanceof ${patternTypeClass};
          if (ir == this.name() || ir == this.tpe()) {
            return (T) ir.mapExpressions(fn);
          }
          """
              .replace("${patternTypeClass}", PATTERN_TYPE_CLASS));
    }
    if (isProcessingPatternConstructor()) {
      specialHandling.append(
          """
          // Special case - `constructor` of `Pattern.Constructor` is not processed.
          // This means no `fn.apply` call on it.
          assert this instanceof ${patternConstructorClass};
          if (ir == this.constructor()) {
            return (T) ir.mapExpressions(fn);
          }
          """
              .replace("${patternConstructorClass}", PATTERN_CONSTRUCTOR_CLASS));
    }
    var code =
        """
        @SuppressWarnings("unchecked")
        private <T extends IR> T doMapExpr(
            T ir,
            java.util.function.Function<org.enso.compiler.core.ir.Expression, org.enso.compiler.core.ir.Expression> fn) {
          ${specialHandling}
          // Either recurse to `mapExpression` or call `fn.apply` on the expression.
          return switch(ir) {
            case org.enso.compiler.core.ir.Name.MethodReference nameRef -> (T) nameRef.mapExpressions(fn);
            case org.enso.compiler.core.ir.Expression expr -> (T) fn.apply(expr);
            default -> (T) ir.mapExpressions(fn);
          };
        }
        """
            .replace("${specialHandling}", specialHandling.toString());
    return code;
  }

  private List<Field> restOfUserFields(List<MappedChild> newChildren) {
    var restOfFields = new ArrayList<Field>();
    for (var userField : ctx.getUserFields()) {
      if (newChildren.stream()
          .noneMatch(newChild -> newChild.child.getName().equals(userField.getName()))) {
        restOfFields.add(userField);
      }
    }
    return restOfFields;
  }

  private String mapOptionListField(String newVarName, OptionListField field) {
    var newVarType =
        "Option<scala.collection.immutable.List<"
            + field.getNestedTypeParameter().getQualifiedName()
            + ">>";
    var code =
        """
        ${newVarType} ${newVarName} = Option.empty();
        if (${fieldName}.isDefined()) {
          var newList = ${fieldName}.get().map(elem -> doMapExpr(elem, fn));
          ${newVarName} = Option.apply(newList);
        }
        """
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapPersistanceReference(String newVarName, PersistanceReferenceField field) {
    var code =
        """
        var ${newVarName} = org.enso.persist.Persistance.Reference.of(
            doMapExpr(${fieldName}.get(${type}.class), fn)
        );
        """
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName())
            .replace("${type}", field.getTypeParameter().getQualifiedName().toString());
    return code;
  }

  private String mapList(String newVarName, ListField field) {
    var newVarType =
        "scala.collection.immutable.List<" + field.getTypeParameter().getQualifiedName() + ">";
    var code =
        """
        ${newVarType} ${newVarName} = null;
        if (${fieldName} != null) {
          ${newVarName} = ${fieldName}.map(elem -> doMapExpr(elem, fn));
        }
        """
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapOption(String newVarName, OptionField field) {
    var newVarType = "Option<" + field.getTypeParameter().getQualifiedName() + ">";
    var type = field.getTypeParameter().getQualifiedName();
    var code =
        """
        ${newVarType} ${newVarName} = Option.empty();
        if (${fieldName} == null) {
          throw new IllegalStateException(
            "Child of type scala.Option must not be null. But field "
            + "${fieldName} "
            + "was null.");
        }
        if (${fieldName}.isDefined()) {
          var elem = ${fieldName}.get();
          var mapped = doMapExpr(elem, fn);
          ${newVarName} = Option.apply((${type}) mapped);
        }
        """
            .replace("${type}", type.toString())
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapOther(String newVarName, String newVarType, Field field) {
    // These field types are handled above.
    Utils.hardAssert(!(field instanceof ListField));
    Utils.hardAssert(!(field instanceof OptionListField));
    Utils.hardAssert(!(field instanceof OptionField));
    var nullableCheck = "";
    if (!field.isNullable()) {
      nullableCheck =
          """
          if (${fieldName} == null) {
            throw new IllegalStateException(
              "Field ${fieldName} must not be null. It was annotated with "
              + "@IRChild(required = true).");
          }
          """
              .replace("${fieldName}", field.getName());
    }
    var code =
        """
        ${newVarType} ${newVarName} = null;
        ${nullableCheck}
        if (${fieldName} != null) {
          ${newVarName} = doMapExpr(${fieldName}, fn);
        }
        """
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName())
            .replace("${nullableCheck}", nullableCheck);
    return code;
  }

  private record MappedChild(String newChildName, Field child, boolean shouldCast) {}
}
