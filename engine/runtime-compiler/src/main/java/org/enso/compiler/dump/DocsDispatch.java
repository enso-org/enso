package org.enso.compiler.dump;

import java.io.BufferedWriter;
import java.io.IOException;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.pkg.QualifiedName;

/**
 * Class to use from {@link DocsGenerate} to dispatch individual IR elements to provided visitor.
 */
abstract class DocsDispatch {
  static DocsDispatch create(DocsVisit visitor, BufferedWriter writer) {
    return new DocsDispatch() {
      @Override
      boolean dispatchModule(QualifiedName name, Module ir) throws IOException {
        return visitor.visitModule(name, ir, writer);
      }

      @Override
      void dispatchMethod(Method.Explicit m) throws IOException {
        visitor.visitMethod(m, writer);
      }

      @Override
      void dispatchConversion(Method.Conversion c) throws IOException {
        visitor.visitConversion(c, writer);
      }

      @Override
      boolean dispatchType(Definition.Type t) throws IOException {
        return visitor.visitType(t, writer);
      }
    };
  }

  private DocsDispatch() {}

  abstract boolean dispatchModule(QualifiedName name, Module ir) throws IOException;

  abstract void dispatchMethod(Method.Explicit m) throws IOException;

  abstract void dispatchConversion(Method.Conversion c) throws IOException;

  abstract boolean dispatchType(Definition.Type t) throws IOException;
}
