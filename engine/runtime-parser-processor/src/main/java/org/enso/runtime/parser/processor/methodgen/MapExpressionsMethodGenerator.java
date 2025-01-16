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
        .append("Function<Expression, Expression> fn")
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
                  ExecutableElement childsMapExprMethod;
                  if (child.isList() || child.isOption()) {
                    childsMapExprMethod =
                        Utils.findMapExpressionsMethod(
                            child.getTypeParameter(), ctx.getProcessingEnvironment());
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

                  String newChildType = childsMapExprMethodRetType.getSimpleName().toString();
                  if (child.isList()) {
                    newChildType = "List<" + newChildType + ">";
                  } else if (child.isOption()) {
                    newChildType = "Option<" + newChildType + ">";
                  }
                  var childIsExpression =
                      Utils.isExpression(
                          childsMapExprMethodRetType, ctx.getProcessingEnvironment());

                  var newChildName = child.getName() + "Mapped";
                  sb.append("  ").append(newChildType).append(" ").append(newChildName);
                  if (child.isNullable()) {
                    sb.append(" = null;").append(System.lineSeparator());
                    sb.append("  if (")
                        .append(child.getName())
                        .append(" != null) {")
                        .append(System.lineSeparator());
                    if (childIsExpression) {
                      // childMapped = fn.apply(child);
                      sb.append("    ")
                          .append(newChildName)
                          .append(" = fn.apply(")
                          .append(child.getName())
                          .append(");")
                          .append(System.lineSeparator());
                    } else {
                      // childMapped = child.mapExpressions(fn);
                      sb.append("    ")
                          .append(newChildName)
                          .append(".")
                          .append(METHOD_NAME)
                          .append("(fn);")
                          .append(System.lineSeparator());
                    }
                    sb.append("  }").append(System.lineSeparator());
                  } else {
                    if (!child.isList() && !child.isOption()) {
                      if (childIsExpression) {
                        // ChildType childMapped = fn.apply(child);
                        sb.append(" = ")
                            .append("fn.apply(")
                            .append(child.getName())
                            .append(");")
                            .append(System.lineSeparator());
                      } else {
                        // ChildType childMapped = child.mapExpressions(fn);
                        sb.append(" = ")
                            .append(child.getName())
                            .append(".")
                            .append(METHOD_NAME)
                            .append("(fn);")
                            .append(System.lineSeparator());
                      }
                    } else {
                      Utils.hardAssert(child.isList() || child.isOption());
                      // List<ChildType> childMapped = child.map(e -> e.mapExpressions(fn));
                      sb.append(" = ").append(child.getName()).append(".map(e -> ");
                      if (childIsExpression) {
                        // List<ChildType> childMapped = child.map(e -> fn.apply(e));
                        sb.append("fn.apply(e)");
                      } else {
                        // List<ChildType> childMapped = child.map(e -> e.mapExpressions(fn));
                        sb.append("e.").append(METHOD_NAME).append("(fn)");
                      }
                      sb.append(");").append(System.lineSeparator());
                    }
                  }
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

  private record MappedChild(String newChildName, Field child, boolean shouldCast) {}
}
