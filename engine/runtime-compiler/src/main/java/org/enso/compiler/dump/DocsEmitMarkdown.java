package org.enso.compiler.dump;

import java.io.IOException;
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
  public boolean visitUnknown(IR ir, Appendable w) {
    return true;
  }

  @Override
  public boolean visitModule(QualifiedName name, Module module, Appendable w) throws IOException {
    w.append("## Documentation for " + name + "\n");
    writeDocs(module, w);
    return true;
  }

  @Override
  public void visitMethod(Definition.Type t, Method.Explicit m, Appendable w) throws IOException {
    w.append("#### method " + m.methodName().name() + "\n");
    writeDocs(m, w);
  }

  @Override
  public void visitConversion(Method.Conversion c, Appendable w) throws IOException {
    w.append("#### conversion " + c.methodName().name() + "\n");
    writeDocs(c, w);
  }

  private void writeDocs(IR b, Appendable w) throws IOException {
    var option = b.passData().get(DocumentationComments$.MODULE$);
    if (option.isDefined()) {
      var doc = (DocumentationComments.Doc) option.get();
      w.append(doc.documentation());
      w.append("\n\n\n");
    }
  }

  @Override
  public boolean visitType(Definition.Type t, Appendable w) throws IOException {
    w.append("#### **type** " + t.name().name() + "\n");
    return true;
  }

  @Override
  public void visitConstructor(Definition.Type t, Definition.Data d, Appendable w)
      throws IOException {
    w.append("#### data " + d.name().name() + "\n");
  }
}
