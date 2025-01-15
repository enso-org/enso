module org.enso.runtime.parser.processor {
  requires java.compiler;
  requires org.enso.runtime.parser.dsl;

  provides javax.annotation.processing.Processor with
      org.enso.runtime.parser.processor.IRProcessor;
}
