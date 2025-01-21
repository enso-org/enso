package org.enso.compiler.dump;

import java.io.IOException;
import java.io.PrintWriter;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.resolve.DocumentationComments;
import org.enso.compiler.pass.resolve.DocumentationComments$;
import org.enso.pkg.QualifiedName;

/** Visitor that emits documentation in markdown format. */
final class DocsEmitMarkdown implements DocsVisit {

  @Override
  public boolean visitUnknown(IR ir, PrintWriter w) {
    return true;
  }

  @Override
  public boolean visitModule(QualifiedName name, Module module, PrintWriter w) throws IOException {
    w.println("## Documentation for " + name);
    writeDocs(module, w);
    return true;
  }

  @Override
  public void visitMethod(Definition.Type t, Method.Explicit m, PrintWriter w) throws IOException {
    w.println("#### method " + m.methodName().name());
    writeDocs(m, w);
  }

  @Override
  public void visitConversion(Method.Conversion c, PrintWriter w) throws IOException {
    w.println("#### conversion " + c.methodName().name());
    writeDocs(c, w);
  }

  private void writeDocs(IR b, PrintWriter w) throws IOException {
    var option = b.passData().get(DocumentationComments$.MODULE$);
    if (option.isDefined()) {
      var doc = (DocumentationComments.Doc) option.get();
      w.println(doc.documentation());
      w.println();
      w.println();
    }
  }

  @Override
  public boolean visitType(Definition.Type t, PrintWriter w) throws IOException {
    w.println("#### **type** " + t.name().name());
    return true;
  }

  @Override
  public void visitConstructor(Definition.Type t, Definition.Data d, PrintWriter w)
      throws IOException {
    w.println("#### data " + d.name().name());
  }
}
