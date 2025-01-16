package org.enso.compiler.dump.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

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
import org.enso.test.utils.ProjectUtils;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class DocsGenerateTest {
  @ClassRule public static final TemporaryFolder TEMP = new TemporaryFolder();

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

        main = Calc.create 42
        """;

    var v = new MockVisitor();

    generateDocumentation("Calc", code, v);

    assertEquals("One type found", 1, v.visitType.size());
    assertEquals("Three constructors", 3, v.visitConstructor.size());

    var typeMethods =
        v.visitMethod.stream()
            .filter(p -> p.t() != null)
            .map(
                p -> {
                  assertEquals("Name of the type is", "Calc", p.t().name().name());
                  return p;
                })
            .toList();
    var moduleMethods = new ArrayList<>(v.visitMethod);
    moduleMethods.removeAll(typeMethods);

    assertEquals(
        "Two type methods: "
            + typeMethods.stream()
                .map(
                    p -> {
                      var typePref = p.t() != null ? p.t().name().name() + "." : "";
                      return typePref + p.ir().methodName().name();
                    })
                .toList(),
        2,
        typeMethods.size());

    assertEquals("One module method", 1, moduleMethods.size());
    assertEquals("main", moduleMethods.get(0).ir().methodName().name());
  }

  private static void generateDocumentation(String name, String code, DocsVisit v)
      throws IOException {
    var pathCalc = TEMP.newFolder(name);
    ProjectUtils.createProject(name, code, pathCalc.toPath());
    ProjectUtils.generateProjectDocs(
        ContextUtils.defaultContextBuilder(),
        pathCalc.toPath(),
        (context) -> {
          var enso = ContextUtils.leakContext(context);
          var modules = enso.getTopScope().getModules();
          var optMod =
              modules.stream().filter(m -> m.getName().toString().contains(name)).findFirst();
          assertTrue(
              "Found " + name + " in " + modules.stream().map(m -> m.getName()).toList(),
              optMod.isPresent());
          var mod = optMod.get();
          assertEquals("local." + name + ".Main", mod.getName().toString());
          var ir = mod.getIr();
          assertNotNull("Ir for " + mod + " found", ir);

          try {
            DocsGenerate.visitModule(v, mod.getName(), ir, null);
          } catch (IOException e) {
            throw raise(RuntimeException.class, e);
          }
        });
  }

  private static final class MockVisitor implements DocsVisit {
    private final List<Module> visitModule = new ArrayList<>();
    private final List<Definition.Type> visitType = new ArrayList<>();
    private final List<Definition.Data> visitConstructor = new ArrayList<>();
    private final List<IR> visitUnknown = new ArrayList<>();
    private final List<TypeAnd<Method.Explicit>> visitMethod = new ArrayList<>();
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
    public void visitMethod(Definition.Type t, Method.Explicit m, Appendable writer)
        throws IOException {
      visitMethod.add(new TypeAnd<>(t, m));
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

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Exception t) throws E {
    throw (E) t;
  }

  record TypeAnd<IRElement>(Definition.Type t, IRElement ir) {}
}
