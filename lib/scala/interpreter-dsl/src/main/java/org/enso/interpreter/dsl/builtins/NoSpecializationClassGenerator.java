package org.enso.interpreter.dsl.builtins;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ExecutableElement;
import javax.tools.Diagnostic.Kind;

/** Generator for builtin method class with no specialization. */
public final class NoSpecializationClassGenerator extends MethodNodeClassGenerator {

  ExecutableElement origin;
  int varArgExpansion;
  private final ProcessingEnvironment processingEnvironment;

  public NoSpecializationClassGenerator(
      ProcessingEnvironment processingEnvironment,
      ExecutableElement origin,
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner,
      int varArgExpansion) {
    super(builtinNode, ownerClazz, stdlibOwner);
    this.origin = origin;
    this.varArgExpansion = varArgExpansion;
    this.processingEnvironment = processingEnvironment;
  }

  public NoSpecializationClassGenerator(
      ProcessingEnvironment processingEnvironment,
      ExecutableElement origin,
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner) {
    this(processingEnvironment, origin, builtinNode, ownerClazz, stdlibOwner, 0);
  }

  @Override
  protected MethodGenerator methodsGen() {
    var asGuestValue = needsGuestValueConversion(origin);
    if (asGuestValue
        && TypeWithKind.isTruffleObject(processingEnvironment, origin.getReturnType())) {
      processingEnvironment
          .getMessager()
          .printMessage(
              Kind.ERROR,
              "Value is already TruffleObject, don't use @Builtin.ReturningGuestObject",
              origin);
    }
    return new ExecuteMethodImplGenerator(
        processingEnvironment, origin, asGuestValue, varArgExpansion);
  }

  @Override
  protected boolean isAbstract() {
    return false;
  }
}
