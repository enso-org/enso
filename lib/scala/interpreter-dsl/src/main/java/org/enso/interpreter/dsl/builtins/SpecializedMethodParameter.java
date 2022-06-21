package org.enso.interpreter.dsl.builtins;

import org.enso.interpreter.dsl.BuiltinsProcessor;

import java.util.Optional;
import java.util.stream.Stream;

abstract class SpecializedMethodParameter {
  abstract String paramName();

  abstract Optional<String> auxParamDef();

  abstract Stream<String> declaredParameter();

  static SpecializedMethodParameter paramOfSpecializedMethod(
      MethodParameter param, Optional<Integer> specializedOnParam) {
    if (specializedOnParam.map(p -> p == param.index()).orElse(false)) {
      if (param.tpe().equals("java.lang.String")) {
        return new CachedMethodParameter(param, "build()", "ExpectStringNode", "expectStringNode");
      } else {
        return new RegularMethodParameter(param);
      }
    } else {
      if (param.needsToInjectValueOfType()) {
        return new InjectedMethodParameter(param);
      } else {
        return new RegularMethodParameter(param);
      }
    }
  }
}

class RegularMethodParameter extends SpecializedMethodParameter {
  public RegularMethodParameter(MethodParameter param) {
    this.param = param;
  }

  private final MethodParameter param;

  @Override
  String paramName() {
    return param.name();
  }

  @Override
  Optional<String> auxParamDef() {
    return Optional.empty();
  }

  @Override
  Stream<String> declaredParameter() {
    return param.declaredParameters(Optional.empty());
  }
}

class CachedMethodParameter extends SpecializedMethodParameter {
  private final MethodParameter param;
  private final String cachedExpr;
  private final String cacheNode;
  private final String cacheNodeParam;

  public CachedMethodParameter(
      MethodParameter param, String cachedExpr, String cacheNode, String cacheNodeParam) {
    this.param = param;
    this.cachedExpr = cachedExpr;
    this.cacheNode = cacheNode;
    this.cacheNodeParam = cacheNodeParam;
  }

  @Override
  String paramName() {
    return param.name() + "Cached";
  }

  @Override
  public Optional<String> auxParamDef() {
    return Optional.of(
        param.tpe()
            + " "
            + paramName()
            + " = "
            + cacheNodeParam
            + ".execute("
            + param.name()
            + ");");
  }

  @Override
  public Stream<String> declaredParameter() {
    return Stream.of(
        "Object " + param.name(),
        "@Cached(\"" + cachedExpr + "\") " + cacheNode + " " + cacheNodeParam);
  }
}

class InjectedMethodParameter extends SpecializedMethodParameter {
  private final MethodParameter param;

  public InjectedMethodParameter(MethodParameter param) {
    this.param = param;
  }

  @Override
  String paramName() {
    return param.name();
  }

  @Override
  public Optional<String> auxParamDef() {
    return Optional.of("Context " + param.name() + " = Context.get(this);");
  }

  @Override
  public Stream<String> declaredParameter() {
    return Stream.empty();
  }
}
