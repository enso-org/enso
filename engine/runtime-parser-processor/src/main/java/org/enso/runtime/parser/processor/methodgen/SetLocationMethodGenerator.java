package org.enso.runtime.parser.processor.methodgen;

import javax.lang.model.element.ExecutableElement;
import org.enso.runtime.parser.processor.GeneratedClassContext;
import org.enso.runtime.parser.processor.IRProcessingException;

public class SetLocationMethodGenerator {
  private final ExecutableElement setLocationMethod;
  private final GeneratedClassContext ctx;

  public SetLocationMethodGenerator(
      ExecutableElement setLocationMethod, GeneratedClassContext ctx) {
    ensureCorrectSignature(setLocationMethod);
    this.ctx = ctx;
    this.setLocationMethod = setLocationMethod;
  }

  private static void ensureCorrectSignature(ExecutableElement setLocationMethod) {
    if (!setLocationMethod.getSimpleName().toString().equals("setLocation")) {
      throw new IRProcessingException(
          "setLocation method must be named setLocation, but was: " + setLocationMethod,
          setLocationMethod);
    }
    if (setLocationMethod.getParameters().size() != 1) {
      throw new IRProcessingException(
          "setLocation method must have exactly one parameter, but had: "
              + setLocationMethod.getParameters(),
          setLocationMethod);
    }
  }

  public String generateMethodCode() {
    var code =
        """
        @Override
        public $retType setLocation(Option<IdentifiedLocation> location) {
          IdentifiedLocation loc = null;
          if (location.isDefined()) {
            loc = location.get();
          }
          return builder().location(loc).build();
        }
        """
            .replace("$retType", retType());
    return code;
  }

  private String retType() {
    return ctx.getProcessedClass().getClazz().getSimpleName().toString();
  }
}
