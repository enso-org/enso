package org.enso.compiler.dump;

import java.io.IOException;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.pkg.QualifiedName;

/**
 * Visitor for processing documentation elements in a module. Pass into {@link
 * DocsGenerate#visitModule}. This interface also includes various static methods to help working
 * with the {@link IR}.
 */
public interface DocsVisit {
  boolean visitModule(QualifiedName name, Module ir, Appendable writer) throws IOException;

  boolean visitUnknown(IR ir, Appendable w) throws IOException;

  void visitMethod(Definition.Type t, Method.Explicit m, Appendable writer) throws IOException;

  void visitConversion(Method.Conversion c, Appendable w) throws IOException;

  boolean visitType(Definition.Type t, Appendable w) throws IOException;

  void visitConstructor(Definition.Type t, Definition.Data d, Appendable w) throws IOException;

  /**
   * Converts a method into textual representation of its signature.
   *
   * @param method the method to process
   * @return text representing the method name and its signature (if any)
   */
  public static String toSignature(Method.Explicit method) {
    return DocsUtils.toSignature(method);
  }

  /**
   * Converts a constructor into textual representation of its signature.
   *
   * @param cons the constructor to process
   * @return text representing the constructor name and its signature (if any)
   */
  public static String toSignature(Definition.Data cons) {
    return DocsUtils.toSignature(cons);
  }
}
