package org.enso.interpreter.dsl;

import com.google.auto.service.AutoService;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import java.util.Set;

@SupportedAnnotationTypes(
    "org.enso.interpreter.dsl.BuiltinMethod")
@SupportedSourceVersion(SourceVersion.RELEASE_8)
@AutoService(Processor.class)
public class MethodProcessor extends AbstractProcessor {

  @Override
  public boolean process(Set<? extends TypeElement> annotations,
                         RoundEnvironment roundEnv) {
    processingEnv.getMessager().printMessage(Diagnostic.Kind.WARNING, "I'm running and I ain't stoppin'");
    return false;
  }
}