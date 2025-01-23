package org.enso.compiler.dump;

import static org.enso.scala.wrapper.ScalaConversions.asJava;

import java.io.IOException;
import java.io.PrintWriter;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.pkg.QualifiedName;

/** Visitor that emits documentation in markdown format. */
final class DocsEmitSignatures implements DocsVisit {

  @Override
  public boolean visitUnknown(IR ir, PrintWriter w) throws IOException {
    w.println("- Unknown IR " + ir.getClass().getName());
    return true;
  }

  @Override
  public boolean visitModule(QualifiedName name, Module module, PrintWriter w) throws IOException {
    w.println("## Enso Signatures 1.0");
    w.println("## module " + name);
    return true;
  }

  @Override
  public void visitMethod(Definition.Type t, Method.Explicit m, PrintWriter w) throws IOException {
    if (t != null) {
      w.append("    - ");
    } else {
      if (m.typeName().isDefined()) {
        var fqn = DocsUtils.toFqnOrSimpleName(m.typeName().get());
        w.append(fqn + ".");
      }
    }
    w.println(DocsVisit.toSignature(m));
  }

  @Override
  public void visitConversion(Method.Conversion c, PrintWriter w) throws IOException {
    assert c.typeName().isDefined() : "Conversions need type name: " + c;
    var fqn = DocsUtils.toFqnOrSimpleName(c.typeName().get());
    w.append(fqn + ".");
    w.append(DocsVisit.toSignature(c));
    w.append(" -> ").println(fqn);
  }

  @Override
  public boolean visitType(Definition.Type t, PrintWriter w) throws IOException {
    var sb = new StringBuilder();
    sb.append("- type ").append(t.name().name());
    for (var a : asJava(t.params())) {
      sb.append(" ").append(DocsVisit.toSignature(a));
    }
    w.println(sb.toString());
    return true;
  }

  @Override
  public void visitConstructor(Definition.Type t, Definition.Data d, PrintWriter w)
      throws IOException {
    w.println("    - " + DocsVisit.toSignature(d));
  }
}
