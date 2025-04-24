package org.enso.compiler.docs;

import static org.enso.compiler.MetadataInteropHelpers.getMetadataOrNull;

import java.io.IOException;
import java.io.PrintWriter;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.analyse.types.InferredType;
import org.enso.compiler.pass.analyse.types.TypeInferencePropagation;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

public class TypesCoverageVisit implements DocsVisit {
  @Override
  public boolean visitModule(QualifiedName name, Module ir, PrintWriter writer) throws IOException {
    writer.println("method,bindings,checked bindings");
    return true;
  }

  @Override
  public boolean visitUnknown(IR ir, PrintWriter w) throws IOException {
    return true;
  }

  @Override
  public void visitMethod(Definition.Type t, Method.Explicit m, PrintWriter writer)
      throws IOException {
    if (t != null) {
      writer.print(t.name().name());
      writer.print(".");
    }
    writer.print(m.methodName().name());
    writer.print(",");

    count(m.body()).print(writer);
    writer.println();
  }

  @Override
  public void visitConversion(Method.Conversion c, PrintWriter w) throws IOException {
    if (c.typeName().isDefined()) {
      w.print(c.typeName().get().name());
      w.print(".");
    }
    w.print(c.methodName().name());
    w.print(",");

    count(c.body()).print(w);
    w.println();
  }

  private record BindingsCount(int all, int checked) {
    void print(PrintWriter w) {
      w.print(all);
      w.print(",");
      w.print(checked);
    }
  }

  private static class BindingsCounter {
    private int all = 0;
    private int checked = 0;

    void count(IR b) {
      IR.preorder(
          b,
          (expr) -> {
            if (expr instanceof Expression.Binding binding) {
              all++;
              var inferredType =
                  getMetadataOrNull(binding, TypeInferencePropagation.INSTANCE, InferredType.class);
              if (inferredType != null && !inferredType.type().equals(TypeRepresentation.UNKNOWN)) {
                checked++;
              }
            }
          });
    }

    BindingsCount finish() {
      return new BindingsCount(all, checked);
    }
  }

  private BindingsCount count(IR b) {
    var counter = new BindingsCounter();
    counter.count(b);
    return counter.finish();
  }

  @Override
  public boolean visitType(Definition.Type t, PrintWriter w) throws IOException {
    return true;
  }

  @Override
  public void visitConstructor(Definition.Type t, Definition.Data d, PrintWriter w)
      throws IOException {}
}
