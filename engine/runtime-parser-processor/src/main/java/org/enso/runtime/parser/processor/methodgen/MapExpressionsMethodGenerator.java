package org.enso.runtime.parser.processor.methodgen;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import javax.lang.model.element.ExecutableElement;
import org.enso.runtime.parser.processor.ClassField;
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
    var subclassType = ctx.getProcessedClass().getClazz().getSimpleName().toString();
    sb.append("@Override").append(System.lineSeparator());
    sb.append("public ")
        .append(subclassType)
        .append(" ")
        .append(METHOD_NAME)
        .append("(")
        .append("java.util.function.Function<Expression, Expression> fn")
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
                  var isChildExpression =
                      Utils.isExpression(
                          childsMapExprMethodRetType, ctx.getProcessingEnvironment());

                  String newChildType = childsMapExprMethodRetType.getSimpleName().toString();

                  var newChildName = child.getName() + "Mapped";
                  var mapCode =
                      switch (child) {
                        case PersistanceReferenceField perRefField -> mapPersistanceReference(
                            newChildName, perRefField);
                        case ListField listField -> mapList(
                            newChildName, isChildExpression, listField);
                        case OptionField optionField -> mapOption(
                            newChildName, isChildExpression, optionField);
                        case OptionListField optionListField -> mapOptionListField(
                            newChildName, isChildExpression, optionListField);
                        default -> mapOther(newChildName, newChildType, isChildExpression, child);
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
          .append(ctx.getProcessedClass().getClazz().getSimpleName().toString())
          .append(") this;")
          .append(System.lineSeparator());
      sb.append("}").append(System.lineSeparator());
      return sb.toString();
    }
    sb.append("  // Only copy if some of the children actually changed")
        .append(System.lineSeparator());
    var changedCond =
        newChildren.stream()
            .map(newChild -> newChild.newChildName + " != " + newChild.child.getName())
            .collect(Collectors.joining(" || "));
    sb.append("  ").append("if (").append(changedCond).append(") {").append(System.lineSeparator());
    sb.append("    ").append("var bldr = new Builder();").append(System.lineSeparator());
    for (MappedChild newChild : newChildren) {
      if (newChild.shouldCast) {
        sb.append("    ")
            .append("if (!(")
            .append(newChild.newChildName)
            .append(" instanceof ")
            .append(newChild.child.getSimpleTypeName())
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
        sb.append("(").append(newChild.child.getSimpleTypeName()).append(") ");
      }
      sb.append(newChild.newChildName).append(");").append(System.lineSeparator());
    }
    for (var field : restOfTheFields(newChildren)) {
      sb.append("    ")
          .append("bldr.")
          .append(field.name())
          .append("(")
          .append(field.name())
          .append(");")
          .append(System.lineSeparator());
    }
    sb.append("    return bldr.build();").append(System.lineSeparator());
    sb.append("  } else { ").append(System.lineSeparator());
    sb.append("    // None of the mapped children changed - just return this")
        .append(System.lineSeparator());
    sb.append("    return ")
        .append("(")
        .append(ctx.getProcessedClass().getClazz().getSimpleName().toString())
        .append(") this;")
        .append(System.lineSeparator());
    sb.append("  }").append(System.lineSeparator());
    sb.append("}").append(System.lineSeparator());
    return sb.toString();
  }

  private List<ClassField> restOfTheFields(List<MappedChild> newChildren) {
    var restOfFields = new ArrayList<ClassField>();
    for (var field : ctx.getAllFields()) {
      if (newChildren.stream()
          .noneMatch(newChild -> newChild.child.getName().equals(field.name()))) {
        restOfFields.add(field);
      }
    }
    return restOfFields;
  }

  private String mapOptionListField(
      String newVarName, boolean isChildExpression, OptionListField field) {
    var newVarType =
        "Option<List<" + field.getNestedTypeParameter().getSimpleName().toString() + ">>";
    String mapExpr;
    if (isChildExpression) {
      mapExpr = "fn.apply(elem)";
    } else {
      mapExpr = "elem." + METHOD_NAME + "(fn)";
    }
    var code =
        """
        ${newVarType} ${newVarName} = Option.empty();
        if (${fieldName}.isDefined()) {
          var newList = ${fieldName}.get().map(elem -> ${mapExpr});
          ${newVarName} = Option.apply(newList);
        }
        """
            .replace("${mapExpr}", mapExpr)
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapPersistanceReference(String newVarName, PersistanceReferenceField field) {
    var code =
        """
        var ${newVarName} = Reference.of(
            ${fieldName}.get(${type}.class).${methodName}(fn));
        """
            .replace("${newVarName}", newVarName)
            .replace("${methodName}", METHOD_NAME)
            .replace("${fieldName}", field.getName())
            .replace("${type}", field.getTypeParameter().getSimpleName().toString());
    return code;
  }

  private String mapList(String newVarName, boolean isChildExpression, ListField field) {
    String mapExpr;
    if (isChildExpression) {
      mapExpr = "fn.apply(elem)";
    } else {
      mapExpr = "elem." + METHOD_NAME + "(fn)";
    }
    var newVarType = "List<" + field.getTypeParameter().getSimpleName().toString() + ">";
    var code =
        """
        ${newVarType} ${newVarName} = null;
        if (${fieldName} != null) {
          ${newVarName} = ${fieldName}.map(elem -> ${mapExpr});
        }
        """
            .replace("${newVarType}", newVarType)
            .replace("${mapExpr}", mapExpr)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapOption(String newVarName, boolean isChildExpression, OptionField field) {
    var newVarType = "Option<" + field.getTypeParameter().getSimpleName().toString() + ">";
    var type = field.getTypeParameter().getSimpleName();
    String mapExpr;
    if (isChildExpression) {
      mapExpr = "fn.apply(elem)";
    } else {
      mapExpr = "elem." + METHOD_NAME + "(fn)";
    }
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
          var mapped = ${mapExpr};
          ${newVarName} = Option.apply((${type}) mapped);
        }
        """
            .replace("${mapExpr}", mapExpr)
            .replace("${type}", type.toString())
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName());
    return code;
  }

  private String mapOther(
      String newVarName, String newVarType, boolean childIsExpression, Field field) {
    // These field types are handled above.
    Utils.hardAssert(!(field instanceof ListField));
    Utils.hardAssert(!(field instanceof OptionListField));
    Utils.hardAssert(!(field instanceof OptionField));
    var nullableCheck = "";
    if (field.isNullable()) {
      nullableCheck =
          """
          if (${fieldName} == null) {
            throw new IllegalStateException(
              "Field ${fieldName} must not be null. It was annotated with "
              + "@IRChild(required = true).");
          }
          """;
    }
    String mapLine;
    if (childIsExpression) {
      mapLine = "fn.apply(" + field.getName() + ");" + System.lineSeparator();
    } else {
      mapLine = field.getName() + "." + METHOD_NAME + "(fn);" + System.lineSeparator();
    }
    var code =
        """
        ${newVarType} ${newVarName} = null;
        ${nullableCheck}
        if (${fieldName} != null) {
          ${newVarName} = ${mapLine}
        }
        """
            .replace("${mapLine}", mapLine)
            .replace("${newVarType}", newVarType)
            .replace("${newVarName}", newVarName)
            .replace("${fieldName}", field.getName())
            .replace("${nullableCheck}", nullableCheck);
    return code;
  }

  private record MappedChild(String newChildName, Field child, boolean shouldCast) {}
}
