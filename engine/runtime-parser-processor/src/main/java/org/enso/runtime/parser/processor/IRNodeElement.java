package org.enso.runtime.parser.processor;

import java.util.stream.Collectors;
import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.element.TypeElement;

/**
 * Representation of an interface annotated with {@link org.enso.runtime.parser.dsl.IRNode}. Takes
 * care of - Methods from {@code org.enso.compiler.core.IR} with no default implementation. -
 * Methods annotated with {@link org.enso.runtime.parser.dsl.IRChild} (child elements). - Other
 * methods (will be fields of the record, not children).
 */
final class IRNodeElement {
  private final ProcessingEnvironment processingEnv;

  static final String IMPORTS =
      """
import java.util.UUID;
import java.util.function.Function;
import org.enso.compiler.core.Identifier;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DiagnosticStorage;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import scala.Option;
import scala.collection.immutable.List;
      """;

  IRNodeElement(ProcessingEnvironment processingEnv, TypeElement irNodeInterface) {
    this.processingEnv = processingEnv;
    var elemsInInterface = irNodeInterface.getEnclosedElements();
  }

  /**
   * Returns a String representing all the overriden methods from {@link org.enso.compiler.core.IR}.
   * Meant to be inside the generated record definition.
   */
  String overrideIRMethods() {
    var code =
        """

        @Override
        public MetadataStorage passData() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public Option<IdentifiedLocation> location() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR setLocation(Option<IdentifiedLocation> location) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR mapExpressions(Function<Expression, Expression> fn) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public List<IR> children() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public @Identifier UUID getId() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public DiagnosticStorage diagnostics() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public DiagnosticStorage getDiagnostics() {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public IR duplicate(
          boolean keepLocations,
          boolean keepMetadata,
          boolean keepDiagnostics,
          boolean keepIdentifiers
        ) {
          throw new UnsupportedOperationException("unimplemented");
        }

        @Override
        public String showCode(int indent) {
          throw new UnsupportedOperationException("unimplemented");
        }
        """;
    var indentedCode = code.lines().map(line -> "  " + line).collect(Collectors.joining("\n"));
    return indentedCode;
  }
}
