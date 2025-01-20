package org.enso.compiler.dump.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.PrintWriter;
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

  @Test
  public void functionArgumentTypes() throws Exception {
    var code =
        """
        from Standard.Base import Integer

        sum x:Integer y:Integer -> Integer = x+y
        """;

    var v = new MockVisitor();
    generateDocumentation("Sum", code, v);

    assertEquals("One method only", 1, v.visitMethod.size());
    assertNull("No type associated", v.visitMethod.get(0).t());
    var sum = v.visitMethod.get(0).ir();
    assertEquals(
        "sum x:Standard.Base.Data.Numbers.Integer y:Standard.Base.Data.Numbers.Integer ->"
            + " Standard.Base.Data.Numbers.Integer",
        DocsVisit.toSignature(sum));
  }

  @Test
  public void suspendAndDefault() throws Exception {
    var code =
        """
        from Standard.Base import Integer

        sum ~x:Integer y:Integer=10 = x+y
        """;

    var v = new MockVisitor();
    generateDocumentation("Suspend", code, v);

    assertEquals("One method only", 1, v.visitMethod.size());
    assertNull("No type associated", v.visitMethod.get(0).t());
    var sum = v.visitMethod.get(0).ir();
    assertEquals(
        "sum ~x:Standard.Base.Data.Numbers.Integer y:Standard.Base.Data.Numbers.Integer= ->"
            + " Standard.Base.Any.Any",
        DocsVisit.toSignature(sum));
  }

  @Test
  public void constructorSignature() throws Exception {
    var code =
        """
        from Standard.Base import Integer

        type Result
            Sum ~x:Integer y:Integer=10
        """;

    var v = new MockVisitor();
    generateDocumentation("TypeResult", code, v);

    assertEquals("No methods", 0, v.visitMethod.size());
    assertEquals("One constructor", 1, v.visitConstructor.size());
    var sum = v.visitConstructor.get(0);
    assertEquals(
        "Sum ~x:Standard.Base.Data.Numbers.Integer y:Standard.Base.Data.Numbers.Integer=",
        DocsVisit.toSignature(sum));
  }

  @Test
  public void instanceMethodSignature() throws Exception {
    var code =
        """
        from Standard.Base import Integer

        type Result
            sum self y = self+y
        """;

    var v = new MockVisitor();
    generateDocumentation("InstanceResult", code, v);

    assertEquals("No methods", 1, v.visitMethod.size());
    var p = v.visitMethod.get(0);
    assertEquals("Result", p.t().name().name());
    var sum = p.ir();
    assertEquals(
        "sum self y:Standard.Base.Any.Any -> Standard.Base.Any.Any", DocsVisit.toSignature(sum));
  }

  @Test
  public void staticMethodSignature() throws Exception {
    var code =
        """
        from Standard.Base import Integer

        type Result
            sum ~x:Integer y:Integer=10 = x+y
        """;

    var v = new MockVisitor();
    generateDocumentation("PrivateResult", code, v);

    assertEquals("One sum method", 1, v.visitMethod.size());
    var p = v.visitMethod.get(0);
    assertEquals("Result", p.t().name().name());
    var sum = p.ir();
    assertEquals(
        "sum ~x:Standard.Base.Data.Numbers.Integer y:Standard.Base.Data.Numbers.Integer= ->"
            + " Standard.Base.Any.Any",
        DocsVisit.toSignature(sum));
  }

  @Test
  public void privateAreHidden() throws Exception {
    var code =
        """
        type Result
            private Zero
            private One x

            private create v = Result.One v
            private power self = self.x*self.x
        """;

    var v = new MockVisitor();
    generateDocumentation("StaticResult", code, v);

    assertEquals("No methods", 0, v.visitMethod.size());
    assertEquals("No constructors", 0, v.visitConstructor.size());
  }

  @Test
  public void vectorWithElements() throws Exception {
    var code =
        """
        from Standard.Base import Vector, Text

        values a:Text -> Vector Text = [a]
        """;

    var v = new MockVisitor();
    generateDocumentation("VectorText", code, v);

    assertEquals("One methods", 1, v.visitMethod.size());
    assertEquals("No constructors", 0, v.visitConstructor.size());

    var p = v.visitMethod.get(0);
    assertNull("It is a module method", p.t());

    var m = p.ir();
    assertEquals("values", m.methodName().name());
    assertEquals(
        "Generates vector with argument type as return type",
        "values a:Standard.Base.Data.Text.Text -> (Standard.Base.Data.Vector.Vector"
            + " Standard.Base.Data.Text.Text)",
        DocsVisit.toSignature(m));
  }

  @Test
  public void unionTypes() throws Exception {
    var code =
        """
        type A
        type B
        type C

        one a:A -> A | B | C = A
        """;

    var v = new MockVisitor();
    generateDocumentation("Union", code, v);

    assertEquals("One methods", 1, v.visitMethod.size());
    assertEquals("No constructors", 0, v.visitConstructor.size());

    var p = v.visitMethod.get(0);
    assertNull("It is a module method", p.t());

    var m = p.ir();
    assertEquals("one", m.methodName().name());
    assertEquals(
        "Generates vector with argument type as return type",
        "one a:local.Union.Main.A -> (local.Union.Main.A|local.Union.Main.B|local.Union.Main.C)",
        DocsVisit.toSignature(m));
  }

  @Test
  public void intersectionTypes() throws Exception {
    var code =
        """
        type A
        type B
        type C

        one a:A -> A & B & C = a
        """;

    var v = new MockVisitor();
    generateDocumentation("Inter", code, v);

    assertEquals("One methods", 1, v.visitMethod.size());
    assertEquals("No constructors", 0, v.visitConstructor.size());

    var p = v.visitMethod.get(0);
    assertNull("It is a module method", p.t());

    var m = p.ir();
    var sig = DocsVisit.toSignature(m);
    assertEquals("one", m.methodName().name());
    assertEquals(
        "Generates vector with argument type as return type",
        "one a:local.Inter.Main.A -> (local.Inter.Main.A&local.Inter.Main.B&local.Inter.Main.C)",
        sig);
  }

  private static void generateDocumentation(String name, String code, DocsVisit v)
      throws IOException {
    var pathCalc = TEMP.newFolder(name);
    ProjectUtils.createProject(name, code, pathCalc.toPath());
    ProjectUtils.generateProjectDocs(
        "api",
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
    public boolean visitModule(QualifiedName name, Module ir, PrintWriter writer)
        throws IOException {
      visitModule.add(ir);
      return true;
    }

    @Override
    public boolean visitUnknown(IR ir, PrintWriter w) throws IOException {
      visitUnknown.add(ir);
      return true;
    }

    @Override
    public void visitMethod(Definition.Type t, Method.Explicit m, PrintWriter writer)
        throws IOException {
      visitMethod.add(new TypeAnd<>(t, m));
    }

    @Override
    public void visitConversion(Method.Conversion c, PrintWriter w) throws IOException {
      visitConversion.add(c);
    }

    @Override
    public boolean visitType(Definition.Type t, PrintWriter w) throws IOException {
      visitType.add(t);
      return true;
    }

    @Override
    public void visitConstructor(Definition.Type t, Definition.Data d, PrintWriter w)
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
