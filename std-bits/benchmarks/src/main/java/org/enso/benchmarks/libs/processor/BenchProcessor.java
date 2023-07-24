package org.enso.benchmarks.libs.processor;

import java.util.Set;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Completion;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.annotation.processing.SupportedSourceVersion;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.TypeElement;
import org.openide.util.lookup.ServiceProvider;

//@SupportedAnnotationTypes("org.enso.benchmarks.libs.processor.Dummy")
//@SupportedSourceVersion(SourceVersion.RELEASE_17)
@ServiceProvider(service = Processor.class)
public class BenchProcessor extends AbstractProcessor {
  public BenchProcessor() {
    super();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    System.out.println("[mylog] Running BenchProcessor");
    return false;
  }
}
