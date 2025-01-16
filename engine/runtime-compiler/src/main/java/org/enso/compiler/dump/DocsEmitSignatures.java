package org.enso.compiler.dump;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import java.io.IOException;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.pkg.QualifiedName;

/** Visitor that emits documentation in markdown format. */
final class DocsEmitSignatures implements DocsVisit {

  @Override
  public boolean visitUnknown(IR ir, Appendable w) throws IOException {
    w.append("- Unknown IR " + ir.getClass().getName() + "\n");
    return true;
  }

  @Override
  public boolean visitModule(QualifiedName name, Module module, Appendable w) throws IOException {
    w.append("## " + name + "\n");
    return true;
  }

  @Override
  public void visitMethod(Definition.Type t, Method.Explicit m, Appendable w) throws IOException {
    if (t != null) {
      w.append("    ");
    }
    w.append("- " + DocsVisit.toSignature(m) + "\n");
  }

  @Override
  public void visitConversion(Method.Conversion c, Appendable w) throws IOException {
    w.append("#### conversion " + c.methodName().name() + "\n");
  }

  @Override
  public boolean visitType(Definition.Type t, Appendable w) throws IOException {
    var sb = new StringBuilder();
    sb.append("- type ").append(t.name().name());
    for (var a : asJava(t.params())) {
      sb.append(" ").append(DocsVisit.toSignature(a));
    }
    sb.append("\n");
    w.append(sb.toString());
    return true;
  }

  @Override
  public void visitConstructor(Definition.Type t, Definition.Data d, Appendable w)
      throws IOException {
    w.append("    - " + DocsVisit.toSignature(d) + "\n");
  }
}
