package org.enso.compiler.dump.test;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.Compiler;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.dump.DocsGenerate;
import org.enso.compiler.dump.DocsVisit;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

public class DocsGenerateTest {
  private static Context ctx;
  private static EnsoContext leak;
  private static Compiler compiler;

  public DocsGenerateTest() {}

  @BeforeClass
  public static void initCtx() {
    ctx = ContextUtils.defaultContextBuilder().build();
    leak = ContextUtils.leakContext(ctx);
  }

  @AfterClass
  public static void closeCtx() {
    ctx.close();
    ctx = null;
    leak = null;
  }

  @Test
  public void simpleType() throws Exception {
    var code =
        """
        type Calc
            Zero
            One x
            Two x y

            create v = Calc.One v
            sum self = self.x+self.y
        """;
    var ir = ContextUtils.compileModule(ctx, code);
    var v = new MockVisitor();
    DocsGenerate.visitModule(v, QualifiedName.fromString("test.Calc"), ir, null);

    assertEquals("One type found", 1, v.visitType.size());
    assertEquals("Three constructors", 3, v.visitConstructor.size());
  }

  private static final class MockVisitor implements DocsVisit {
    private final List<Module> visitModule = new ArrayList<>();
    private final List<Definition.Type> visitType = new ArrayList<>();
    private final List<Definition.Data> visitConstructor = new ArrayList<>();
    private final List<IR> visitUnknown = new ArrayList<>();
    private final List<Method.Explicit> visitMethod = new ArrayList<>();
    private final List<Method.Conversion> visitConversion = new ArrayList<>();

    @Override
    public boolean visitModule(QualifiedName name, Module ir, Appendable writer)
        throws IOException {
      visitModule.add(ir);
      return true;
    }

    @Override
    public boolean visitUnknown(IR ir, Appendable w) throws IOException {
      visitUnknown.add(ir);
      return true;
    }

    @Override
    public void visitMethod(Method.Explicit m, Appendable writer) throws IOException {
      visitMethod.add(m);
    }

    @Override
    public void visitConversion(Method.Conversion c, Appendable w) throws IOException {
      visitConversion.add(c);
    }

    @Override
    public boolean visitType(Definition.Type t, Appendable w) throws IOException {
      visitType.add(t);
      return true;
    }

    @Override
    public void visitConstructor(Definition.Type t, Definition.Data d, Appendable w)
        throws IOException {
      visitConstructor.add(d);
    }
  }
}
