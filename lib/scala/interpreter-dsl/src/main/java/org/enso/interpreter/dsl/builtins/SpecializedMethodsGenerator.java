package org.enso.interpreter.dsl.builtins;

import com.google.common.base.CaseFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.VariableElement;
import org.apache.commons.lang3.StringUtils;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.Builtin;

/** A method generator for an abstract `execute` and at least a single specialization. */
public final class SpecializedMethodsGenerator extends MethodGenerator {
  private List<ExecutableElement> elements;
  private static final String WithWarningsClassName =
      "org.enso.interpreter.runtime.error.WithWarnings";

  public SpecializedMethodsGenerator(List<ExecutableElement> elements) {
    this(elements, elements.get(0));
  }

  private SpecializedMethodsGenerator(List<ExecutableElement> elements, ExecutableElement first) {
    this(
        elements,
        first.getModifiers().contains(Modifier.STATIC),
        first.getKind() == ElementKind.CONSTRUCTOR,
        first.getAnnotation(Builtin.ReturningGuestObject.class) != null,
        TypeWithKind.createFromTpe(first.getReturnType().toString()));

    // Make sure all methods were defined the same way, except for paramters' types
    assert (allEqual(elements.stream().map(e -> e.getModifiers().contains(Modifier.STATIC))));
    assert (allEqual(elements.stream().map(e -> e.getKind() == ElementKind.CONSTRUCTOR)));
    assert (allEqual(
        elements.stream().map(e -> e.getAnnotation(Builtin.ReturningGuestObject.class) != null)));
    assert (allEqual(
        elements.stream().map(e -> TypeWithKind.createFromTpe(e.getReturnType().toString()))));
  }

  public SpecializedMethodsGenerator(
      List<ExecutableElement> elements,
      boolean isStatic,
      boolean isConstructor,
      boolean convertToGuestValue,
      TypeWithKind returnTpe) {
    super(isStatic, isConstructor, convertToGuestValue, returnTpe);
    this.elements = elements;
  }

  private <T> boolean allEqual(Stream<T> stream) {
    return stream.distinct().count() <= 1;
  }

  @Override
  public List<String> generate(ProcessingEnvironment processingEnv, String name, String owner) {
    SpecializationMeta meta = inferExecuteParameters();
    List<String> result = new ArrayList<>();

    result.add(methodSigDef(owner, meta.execParams(), true));
    result.add("");
    result.addAll(
        paramsOfSpecializedMethods(processingEnv, meta.diffParam)
            .flatMap(
                specializeMethod ->
                    specialize(owner, name, specializeMethod, meta.diffParam).stream())
            .collect(Collectors.toList()));
    return result;
  }

  @Override
  /** Stub. Don't expand vararg parameters in specializations. */
  protected Optional<Integer> expandVararg(int paramsLen, int paramIndex) {
    return Optional.empty();
  }

  /**
   * Infers the list of parameters for specialized methods.
   *
   * @param processingEnv current round processing environment
   * @param specializedParamIdx optional index of parameter on which specialization occurs. Used
   *     optionally for ordering of specialized methods to satisfy @Specialize pre-conditions
   * @return a stream of SpecializeMethod records
   */
  private Stream<SpecializeMethodInfo> paramsOfSpecializedMethods(
      ProcessingEnvironment processingEnv, Optional<Integer> specializedParamIdx) {
    Stream<SpecializeMethodInfo> unsorted =
        elements.stream()
            .map(
                method -> {
                  List<? extends VariableElement> params = method.getParameters();
                  return new SpecializeMethodInfo(
                      method,
                      IntStream.range(0, params.size())
                          .mapToObj(i -> fromVariableElementToMethodParameter(i, params.get(i)))
                          .collect(Collectors.toList()),
                      wrapExceptions(processingEnv, method));
                });
    if (specializedParamIdx.isEmpty()) {
      // No need to sort specializations when only dealing with a single one
      return unsorted;
    } else {
      final int specializedParamIdxFinal = specializedParamIdx.get();
      return unsorted.sorted(
          (a, b) ->
              isLessSpecific(a.params().get(specializedParamIdxFinal))
                  ? 1
                  : isLessSpecific(b.params().get(specializedParamIdxFinal)) ? -1 : 0);
    }
  }

  private record SpecializeMethodInfo(
      ExecutableElement origin,
      List<MethodParameter> params,
      SafeWrapException[] exceptionWrappers) {}

  /**
   * Helper method to ensure that specialized methods are not forced to be defined in a specific
   * order.
   *
   * @param p `execute`'s parameter on which specialization is performed
   * @return true, if the parameter's type means that the specialization which contains it should be
   *     defined later
   */
  private boolean isLessSpecific(MethodParameter p) {
    return p.tpe().equals("java.lang.Object") || p.tpe().equals("java.lang.String");
  }

  private SpecializationMeta inferExecuteParameters() {
    Map<Integer, List<MethodParameter>> paramss =
        elements.stream()
            .flatMap(
                method -> {
                  List<? extends VariableElement> params = method.getParameters();
                  return IntStream.range(0, params.size())
                      .mapToObj(i -> fromVariableElementToMethodParameter(i, params.get(i)));
                })
            .collect(Collectors.groupingBy(p -> p.index()));

    List<Integer> diffParams = new ArrayList<>();
    ArrayList<MethodParameter> execParams = new ArrayList<>();
    ArrayList<MethodParameter> fallbackExecParams = new ArrayList<>();
    paramss.forEach(
        (k, v) -> {
          if (v.size() != elements.size()) {
            throw new RuntimeException(
                "Restriction: Specialized methods have to have equal number of parameters.\n"
                    + "Expected "
                    + elements.size()
                    + ", got "
                    + v.size());
          }

          MethodParameter p = v.get(0);
          if (!p.needsToInjectValueOfType()
              && !p.isTruffleInjectedParam()
              && !p.isTruffleCachedParam()) {
            if (v.size() > 1 && allEqual(v.stream())) {
              execParams.add(p);
            } else {
              // Specialize on the given parameter
              String ensoName = CaseFormat.LOWER_CAMEL.to(CaseFormat.LOWER_UNDERSCORE, p.name());
              List<String> ensoAnnotations = inferAuxEnsoAnnotations(v);
              execParams.add(
                  new MethodParameter(execParams.size() + 1, ensoName, "Object", ensoAnnotations));
              diffParams.add(k);
            }
            fallbackExecParams.add(p);
          }
        });

    if (diffParams.isEmpty()) {
      if (elements.size() == 1) {
        return new SpecializationMeta(execParams, Optional.empty());
      }
      throw new RuntimeException(
          "Could not infer the parameter to specialize on based on parameters' types");
    } else if (diffParams.size() > 1) {
      if (elements.size() == 1) {
        // Fallback, cannot infer specialization automatically but with a single method
        // we can confidently just leave params as-is
        return new SpecializationMeta(fallbackExecParams, Optional.empty());
      }
      throw new RuntimeException(
          "Implementation limitation: Builtins DSL infers specialization on a single parameter. Write Node specialization manually instead");
    }
    return new SpecializationMeta(execParams, Optional.of(diffParams.get(0)));
  }

  private List<String> inferAuxEnsoAnnotations(List<MethodParameter> params) {
    List<String> annotations = new ArrayList<>();
    Optional<MethodParameter> withWarnings =
        params.stream().filter(p -> p.tpe().equals(WithWarningsClassName)).findAny();
    if (withWarnings.isPresent()) {
      annotations.add("@" + AcceptsWarning.class.getName());
    }
    return annotations;
  }

  private record SpecializationMeta(
      List<MethodParameter> execParams, Optional<Integer> diffParam) {}

  /**
   * Generate node's `execute` method definition (return type and necessary parameters). '
   *
   * @param owner owner of the method
   * @return string representation of the `execute` method's definition
   */
  protected List<String> specialize(
      String owner,
      String name,
      SpecializeMethodInfo methodInfo,
      Optional<Integer> specializedParam) {
    List<SpecializedMethodParameter> params1 =
        methodInfo.params().stream()
            .map(p -> SpecializedMethodParameter.paramOfSpecializedMethod(p, specializedParam))
            .collect(Collectors.toList());
    String paramsDef = "";
    boolean includeSelf = !(isStatic || isConstructor);
    if (!params1.isEmpty()) {
      Object[] allParamDef = params1.stream().flatMap(x -> x.declaredParameter()).toArray();
      if (allParamDef.length > 0) {
        paramsDef = (includeSelf ? ", " : "") + StringUtils.join(allParamDef, ", ");
      }
    }
    String thisParamTpe = isStatic || isConstructor ? "Object" : owner;
    String suffix =
        specializedParam.map(idx -> methodInfo.params().get(idx).tpeSimpleName()).orElse("Execute");

    Builtin.Specialize specializeAnnotation =
        methodInfo.origin.getAnnotation(Builtin.Specialize.class);
    String targetAnnotation = specializeAnnotation.fallback() ? "@Fallback" : "@Specialization";
    String selfParamDecl = includeSelf ? thisParamTpe + " self" : "";
    String methodSig =
        targetReturnType(returnTpe) + " do" + suffix + "(" + selfParamDecl + paramsDef + ")";
    String paramsApplied;
    if (params1.isEmpty()) {
      paramsApplied = "";
    } else {
      paramsApplied = StringUtils.join(params1.stream().map(x -> x.paramName()).toArray(), ", ");
    }

    List<String> methodBody = new ArrayList<>();

    methodBody.addAll(
        params1.stream()
            .flatMap(p -> p.auxParamDef().stream())
            .map(d -> "  " + d)
            .collect(Collectors.toList()));

    if (isConstructor) {
      methodBody.add("  return new " + owner + "(" + paramsApplied + ");");
    } else {
      String qual = isStatic ? owner : "self";
      switch (returnTpe.kind()) {
        case VOID:
          methodBody.add("  " + qual + "." + name + "(" + paramsApplied + ");");
          methodBody.add("  return EnsoContext.get(this).getBuiltins().nothing();");
          break;
        case ARRAY:
          methodBody.add(
              "  return new Array((Object[]) " + qual + "." + name + "(" + paramsApplied + "));");
          break;
        default:
          methodBody.add("  return " + qual + "." + name + "(" + paramsApplied + ");");
      }
    }

    List<String> specializationDeclaration = new ArrayList<>();
    specializationDeclaration.add(targetAnnotation);
    specializationDeclaration.add(methodSig + " {");
    if (methodInfo.exceptionWrappers.length != 0) {
      specializationDeclaration.add("  try {");
      for (String line : methodBody) {
        specializationDeclaration.add("  " + line);
      }
      for (int i = 0; i < methodInfo.exceptionWrappers.length; i++) {
        specializationDeclaration.addAll(methodInfo.exceptionWrappers[i].toCatchClause());
      }
      specializationDeclaration.add("  }");
    } else {
      specializationDeclaration.addAll(methodBody);
    }

    specializationDeclaration.add("}");
    return specializationDeclaration;
  }
}
