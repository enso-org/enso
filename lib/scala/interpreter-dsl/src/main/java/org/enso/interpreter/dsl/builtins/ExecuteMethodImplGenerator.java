package org.enso.interpreter.dsl.builtins;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.VariableElement;
import org.apache.commons.lang3.StringUtils;

/** A method generator for a single concrete `execute` method. */
public final class ExecuteMethodImplGenerator extends MethodGenerator {
  private final ExecutableElement method;
  private final int varargExpansion;
  private final boolean needsVarargExpansion;

  public ExecuteMethodImplGenerator(
      ProcessingEnvironment processingEnvironment,
      ExecutableElement method,
      boolean convertToGuestValue,
      int expandedVarargs) {
    this(
        processingEnvironment,
        method,
        method.getReturnType().toString(),
        method.getModifiers().contains(Modifier.STATIC),
        method.getKind() == ElementKind.CONSTRUCTOR,
        convertToGuestValue,
        expandedVarargs,
        method.isVarArgs());
  }

  private ExecuteMethodImplGenerator(
      ProcessingEnvironment processingEnvironment,
      ExecutableElement method,
      String returnTpe,
      boolean isStatic,
      boolean isConstructor,
      boolean convertToGuestValue,
      int expandedVarargs,
      boolean isVarargs) {
    super(
        processingEnvironment,
        isStatic,
        isConstructor,
        convertToGuestValue,
        TypeWithKind.createFromTpe(returnTpe));
    this.method = method;
    this.varargExpansion = expandedVarargs;
    this.needsVarargExpansion = isVarargs && (varargExpansion > 0);
  }

  @Override
  protected Optional<Integer> expandVararg(int paramsLen, int paramIndex) {
    return needsVarargExpansion && paramsLen >= (paramIndex + 1)
        ? Optional.of(varargExpansion)
        : Optional.empty();
  }

  private String[] auxToHostConversions(MethodParameter param) {
    if (param.needsToHostTranslation()) {
      TypeWithKind tpeWithKind = TypeWithKind.createFromTpe(param.tpe());
      String hostParam = param.hostVarName();
      String tmpObject = "itemsOf" + param.capitalizedName();
      return new String[] {
        "Object[] " + tmpObject + " = " + param.name() + ".getItems();",
        tpeWithKind.baseType()
            + "[] "
            + hostParam
            + " = new "
            + tpeWithKind.baseType()
            + "["
            + tmpObject
            + ".length];",
        "for (int i=0; i < " + hostParam + ".length; i++) {",
        "  "
            + hostParam
            + "[i] = ("
            + tpeWithKind.baseType()
            + ") context.getEnvironment().asHostObject("
            + tmpObject
            + "[i]);",
        "}"
      };
    } else {
      return new String[0];
    }
  }

  private String[] bodyBase(String name, String owner, List<MethodParameter> params) {
    int paramsLen = params.size();
    String paramsApplied;
    if (params.isEmpty()) {
      paramsApplied = "";
    } else {
      paramsApplied =
          StringUtils.join(
              params.stream()
                  .flatMap(x -> x.paramUseNames(expandVararg(paramsLen, x.index())))
                  .toArray(),
              ", ");
    }
    if (isConstructor) {
      return new String[] {"  return new " + owner + "(" + paramsApplied + ");"};
    } else {
      String qual = isStatic ? owner : "self";
      switch (returnTpe.kind()) {
        case VOID:
          return new String[] {
            "  " + qual + "." + name + "(" + paramsApplied + ");",
            "  return EnsoContext.get(this).getBuiltins().nothing();"
          };
        case ARRAY:
          return new String[] {
            "  return ArrayLikeHelpers.wrapObjects("
                + qual
                + "."
                + name
                + "("
                + paramsApplied
                + "));"
          };
        default:
          if (returnTpe.isValidGuestType()) {
            return new String[] {"  return " + qual + "." + name + "(" + paramsApplied + ");"};
          } else {
            if (!convertToGuestValue) {
              processingEnvironment
                  .getMessager()
                  .printMessage(
                      javax.tools.Diagnostic.Kind.ERROR,
                      "Cannot generate method body for "
                          + method
                          + " because it returns a host object and convertToGuestValue is false");
            }
            return new String[] {
              "  return context",
              "      .asGuestValue(" + qual + "." + name + "(" + paramsApplied + "));"
            };
          }
      }
    }
  }

  private boolean needsContext(List<MethodParameter> params) {
    boolean result = false;
    // Does the return value need to be translated to a guest value?
    if (!isConstructor && (returnTpe.kind() == TypeKind.OBJECT)) {
      if (!returnTpe.isValidGuestType()) {
        result = true;
      }
    }
    // Do any of params need to be translated to a host object?
    return result || params.stream().anyMatch(p -> p.needsToHostTranslation());
  }

  public List<String> generate(String name, String owner) {
    SafeWrapException[] exceptionWrappers = wrapExceptions(processingEnvironment, method);
    boolean wrapsExceptions = exceptionWrappers.length != 0;
    List<? extends VariableElement> rawParams = method.getParameters();
    List<MethodParameter> params =
        IntStream.range(0, method.getParameters().size())
            .mapToObj(
                i ->
                    fromVariableElementToMethodParameter(
                        processingEnvironment, i, rawParams.get(i)))
            .collect(Collectors.toList());

    String[] body = bodyBase(name, owner, params);

    List<String> method = new ArrayList<>();
    method.add(methodSigDef(owner, params, false) + " {");
    if (needsContext(params)) {
      method.add("  EnsoContext context = EnsoContext.get(this);");
    }
    if (wrapsExceptions) {;
      method.add("  try {");
      params.stream()
          .forEach(
              p -> {
                for (String s : auxToHostConversions(p)) {
                  method.add("    " + s);
                }
              });
      for (String statement : body) {
        method.add("  " + statement);
      }
      for (int i = 0; i < exceptionWrappers.length; i++) {
        method.addAll(exceptionWrappers[i].toCatchClause());
      }
      method.add("  }");
      method.add("}");
    } else {
      params.stream()
          .forEach(
              p -> {
                for (String s : auxToHostConversions(p)) {
                  method.add("    " + s);
                }
              });
      method.addAll(List.of(body));
      method.add("}");
    }
    return method;
  }
}
