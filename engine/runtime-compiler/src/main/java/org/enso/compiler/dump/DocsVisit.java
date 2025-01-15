package org.enso.compiler.dump;

import java.io.IOException;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.pkg.QualifiedName;

interface DocsVisit {
  boolean visitModule(QualifiedName name, Module ir, Appendable writer) throws IOException;

  boolean visitUnknown(IR ir, Appendable w) throws IOException;

  void visitMethod(Method.Explicit m, Appendable writer) throws IOException;

  void visitConversion(Method.Conversion c, Appendable w) throws IOException;

  boolean visitType(Definition.Type t, Appendable w) throws IOException;

  void visitConstructor(Definition.Type t, Definition.Data d, Appendable w) throws IOException;
}
