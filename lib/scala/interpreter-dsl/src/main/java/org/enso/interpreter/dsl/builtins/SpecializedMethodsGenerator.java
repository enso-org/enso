package org.enso.interpreter.dsl.builtins;

import org.apache.commons.lang3.StringUtils;
import org.enso.interpreter.dsl.Builtin;

import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.VariableElement;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/** A method generator for an abstract `execute` and at least a single specialization. */
public final class SpecializedMethodsGenerator extends MethodGenerator {
  private List<ExecutableElement> elements;

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
  public List<String> generate(
      String name, String owner, Map<String, Integer> builtinTypesParameterCounts) {
    SpecializationMeta meta = inferExecuteParameters();
    List<String> result = new ArrayList<>();

    result.add(methodSigDef(owner, meta.execParams(), true));
    result.add("");
    result.addAll(
        paramsOfSpecializedMethods(meta.diffParam)
            .flatMap(paramsList -> specialize(owner, name, paramsList, meta.diffParam).stream())
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
   * @param specializedParamIdx optional index of parameter on which specialization occurs. Used
   *     optionally for ordering of specialized methods to satisfy @Specialize pre-conditions
   * @return a stream of parameters
   */
  private Stream<List<MethodParameter>> paramsOfSpecializedMethods(
      Optional<Integer> specializedParamIdx) {
    Stream<List<MethodParameter>> unsorted =
        elements.stream()
            .map(
                method -> {
                  List<? extends VariableElement> params = method.getParameters();
                  return IntStream.range(0, params.size())
                      .mapToObj(i -> fromVariableElementToMethodParameter(i, params.get(i)))
                      .collect(Collectors.toList());
                });
    if (specializedParamIdx.isEmpty()) {
      // No need to sort specializations when only dealing with a single one
      return unsorted;
    } else {
      final int specializedParamIdxFinal = specializedParamIdx.get();
      return unsorted.sorted(
          (a, b) ->
              isLessSpecific(a.get(specializedParamIdxFinal))
                  ? 1
                  : isLessSpecific(b.get(specializedParamIdxFinal)) ? -1 : 0);
    }
  }

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
          if (!p.needsToInjectValueOfType()) {
            if (v.size() > 1 && allEqual(v.stream())) {
              execParams.add(p);
            } else {
              // Specialize on the given parameter
              execParams.add(
                  new MethodParameter(
                      execParams.size() + 1, p.name(), "Object", new ArrayList<>()));
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

  private record SpecializationMeta(
      List<MethodParameter> execParams, Optional<Integer> diffParam) {}

  /**
   * Generate node's `execute` method definition (return type and necessary parameters). '
   *
   * @param owner owner of the method
   * @return string representation of the `execute` method's definition
   */
  protected List<String> specialize(
      String owner, String name, List<MethodParameter> params, Optional<Integer> specializedParam) {
    List<SpecializedMethodParameter> params1 =
        params.stream()
            .map(p -> SpecializedMethodParameter.paramOfSpecializedMethod(p, specializedParam))
            .collect(Collectors.toList());
    String paramsDef = "";
    if (!params1.isEmpty()) {
      Object[] allParamDef = params1.stream().flatMap(x -> x.declaredParameter()).toArray();
      if (allParamDef.length > 0) {
        paramsDef = ", " + StringUtils.join(allParamDef, ", ");
      }
    }
    String thisParamTpe = isStatic || isConstructor ? "Object" : owner;
    String suffix = specializedParam.map(idx -> params.get(idx).tpeSimpleName()).orElse("Execute");

    String annotation = "@Specialization";
    String methodSig =
        targetReturnType(returnTpe)
            + " do"
            + suffix
            + "("
            + thisParamTpe
            + " _this"
            + paramsDef
            + ")";
    String paramsApplied;
    if (params1.isEmpty()) {
      paramsApplied = "";
    } else {
      paramsApplied = StringUtils.join(params1.stream().map(x -> x.paramName()).toArray(), ", ");
    }

    List<String> specializationDeclaration = new ArrayList<>();
    specializationDeclaration.add(annotation);
    specializationDeclaration.add(methodSig + " {");
    specializationDeclaration.addAll(
        params1.stream()
            .flatMap(p -> p.auxParamDef().stream())
            .map(d -> "  " + d)
            .collect(Collectors.toList()));

    if (isConstructor) {
      specializationDeclaration.add("  return new " + owner + "(" + paramsApplied + ");");
    } else {
      String qual = isStatic ? owner : "_this";
      switch (returnTpe.kind()) {
        case VOID:
          specializationDeclaration.add("  " + qual + "." + name + "(" + paramsApplied + ");");
          specializationDeclaration.add(
              "  return Context.get(this).getBuiltins().nothing().newInstance();");
          break;
        case ARRAY:
          specializationDeclaration.add(
              "  return new Array((Object[]) " + qual + "." + name + "(" + paramsApplied + "));");
          break;
        default:
          specializationDeclaration.add(
              "  return " + qual + "." + name + "(" + paramsApplied + ");");
      }
    }
    specializationDeclaration.add("}");
    return specializationDeclaration;
  }
}
