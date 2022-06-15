package org.enso.interpreter.dsl.builtins;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.ExecutableElement;
import java.util.List;
import java.util.Map;

/**
 * Generator for a builtin method with specializations. The target class will always be abstract and
 * constructed via a static `build` method pattern. The generator will also infer parameters to
 * specialize on and generate methods for them.
 */
public class SpecializationClassGenerator extends MethodNodeClassGenerator {
  List<ExecutableElement> elements;

  public SpecializationClassGenerator(
      List<ExecutableElement> methodElements,
      ClassName builtinNode,
      ClassName ownerClazz,
      ClassName stdlibOwner,
      Map<String, Integer> parameterCounts) {
    super(builtinNode, ownerClazz, stdlibOwner, parameterCounts);
    this.elements = methodElements;
  }

  @Override
  protected MethodGenerator methodsGen() {
    return new SpecializedMethodsGenerator(elements);
  }

  @Override
  protected boolean isAbstract() {
    return true;
  }
}
