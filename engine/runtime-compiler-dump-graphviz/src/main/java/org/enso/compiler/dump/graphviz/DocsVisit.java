package org.enso.compiler.dump;

import java.io.IOException;
import java.io.PrintWriter;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
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
  boolean visitModule(QualifiedName name, Module ir, PrintWriter writer) throws IOException;

  boolean visitUnknown(IR ir, PrintWriter w) throws IOException;

  void visitMethod(Definition.Type t, Method.Explicit m, PrintWriter writer) throws IOException;

  void visitConversion(Method.Conversion c, PrintWriter w) throws IOException;

  boolean visitType(Definition.Type t, PrintWriter w) throws IOException;

  void visitConstructor(Definition.Type t, Definition.Data d, PrintWriter w) throws IOException;

  //
  // helper methods
  //

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

  /**
   * Converts an argument definition into textual representation of its signature.
   *
   * @param arg the argument to process
   * @return text representing the argument
   */
  public static String toSignature(DefinitionArgument arg) {
    return DocsUtils.toSignature(arg);
  }

  //
  // Standard visitor implementations
  //

  /**
   * Generates markdown files with documentation content.
   *
   * @return new instance of visitor generating markdown documentation format
   */
  public static DocsVisit createMarkdown() {
    return new DocsEmitMarkdown();
  }

  /**
   * Ignore comments, but generate fully qualified signatures of the visible elements
   *
   * @return new instance of visitor generating just signatures
   */
  public static DocsVisit createSignatures() {
    return new DocsEmitSignatures();
  }
}
