package org.enso.interpreter.dsl.builtins;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ExecutableElement;
import java.util.Map;

/** Generator for builtin method class with no specialization. */
public final class NoSpecializationClassGenerator extends MethodNodeClassGenerator {

  ExecutableElement origin;
  int varArgExpansion;

  public NoSpecializationClassGenerator(
      ExecutableElement origin,
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner,
      int varArgExpansion,
      Map<String, Integer> builtinTypesParamCount) {
    super(builtinNode, ownerClazz, stdlibOwner, builtinTypesParamCount);
    this.origin = origin;
    this.varArgExpansion = varArgExpansion;
  }

  public NoSpecializationClassGenerator(
      ExecutableElement origin,
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner,
      Map<String, Integer> builtinTypesParamCount) {
    this(origin, builtinNode, ownerClazz, stdlibOwner, 0, builtinTypesParamCount);
  }

  @Override
  protected MethodGenerator methodsGen() {
    return new ExecuteMethodImplGenerator(
        origin, needsGuestValueConversion(origin), varArgExpansion);
  }

  @Override
  protected boolean isAbstract() {
    return false;
  }
}
