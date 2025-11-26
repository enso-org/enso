package org.enso.compiler.dump.test;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.anyOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.docs.DocsGenerate;
import org.enso.compiler.docs.DocsVisit;
import org.enso.editions.LibraryName;
import org.enso.pkg.QualifiedName;
import org.enso.test.utils.ContextUtils;
import org.enso.test.utils.ProjectUtils;
import org.enso.test.utils.SourceModule;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class DocsGenerateTest {
  @ClassRule public static final TemporaryFolder TEMP = new TemporaryFolder();
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  public DocsGenerateTest() {}

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
  public void noSignatureIsGenerated_ForEmptyModule() throws IOException {
    var emptyCode = "";
    var modName = "local.Empty.Main";
    var sig = DumpTestUtils.generateSignatures(ctxRule, emptyCode, modName);
    assertTrue("Empty signature for empty module", sig.isEmpty());
  }

  @Test
  public void noSignatureIsGenerated_ForModuleContainingOnlyImports() throws IOException {
    var codeWithImports =
        """
        import Standard.Base.Any.Any
        import Standard.Base.Data.Vector.Vector
        """;
    var modName = "local.Empty.Main";
    var sig = DumpTestUtils.generateSignatures(ctxRule, codeWithImports, modName);
    assertTrue("Empty signature for module with only imports", sig.isEmpty());
  }

  @Test
  public void generatedSignature_HasCorrectMarkdownFormat() throws IOException {
    var code =
        """
        from Standard.Base import all

        module_method = 42

        type My_Type
            Cons x
            instance_method self = 42

        My_Type.static_method = 42
        Any.extension_method = 42
        My_Type.from (that: Integer) = My_Type.Cons that
        """;
    var modName = "local.Proj.Main";
    var sig = DumpTestUtils.generateSignatures(ctxRule, code, modName);
    sig.lines()
        .forEach(
            line -> {
              assertThat(
                  "Is heading or a list item",
                  line,
                  anyOf(startsWith("#"), startsWith("-"), startsWith("    -")));
            });
  }

  @Test
  public void generatedSignaturesForProject_HasSameDirectoryHierarchyAsSources()
      throws IOException {
    var projName = "Proj";
    var modules =
        Set.of(
            new SourceModule(QualifiedName.fromString("Main"), "main = 42"),
            new SourceModule(QualifiedName.fromString("Subdir.Submodule"), "submodule = 42"));
    var projDir = TEMP.newFolder(projName);
    ProjectUtils.createProject(projName, modules, projDir.toPath());
    ProjectUtils.generateProjectDocs(
        "api",
        ContextUtils.newBuilder(),
        projDir.toPath(),
        ctx -> {
          var ensoCtx = ctx.ensoContext();
          var pkg =
              ensoCtx
                  .getPackageRepository()
                  .getPackageForLibrary(LibraryName.apply("local", projName));
          assertThat(pkg.isDefined(), is(true));
          var signatureOutDir = DocsGenerate.defaultOutputDir(pkg.get());
          assertThat(
              "Default output dir for signatures was created", signatureOutDir.exists(), is(true));
          var srcDir = pkg.get().sourceDir();
          assertThat(srcDir.resolve("Main.enso").exists(), is(true));
          assertThat(signatureOutDir.resolve("Main.md").exists(), is(true));
          assertThat(srcDir.resolve("Subdir").resolve("Submodule.enso").exists(), is(true));
          assertThat(signatureOutDir.resolve("Subdir").resolve("Submodule.md").exists(), is(true));
        });
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

  @Test
  public void typesWithError() throws Exception {
    var code =
        """
        type A
        type B
        type C

        one a:A -> A & B ! C = a
        """;

    var v = new MockVisitor();
    generateDocumentation("Error", code, v);

    assertEquals("One methods", 1, v.visitMethod.size());
    assertEquals("No constructors", 0, v.visitConstructor.size());

    var p = v.visitMethod.get(0);
    assertNull("It is a module method", p.t());

    var m = p.ir();
    var sig = DocsVisit.toSignature(m);
    assertEquals("one", m.methodName().name());
    assertEquals(
        "Generates thrown dataflow errors in the signature",
        "one a:local.Error.Main.A -> (local.Error.Main.A&local.Error.Main.B)!local.Error.Main.C",
        sig);
  }

  @Test
  public void blankArgument_ConsolidatedLambda() throws Exception {
    // This will get consolidated into:
    // foo _ = 42
    // See `LambdaConsolidate` compiler pass
    var code =
        """
        foo =
            _ -> 42
        """;
    var sig = DumpTestUtils.generateSignatures(ctxRule, code, "Main");
    assertSingleBlankArgument("foo", sig);
  }

  @Test
  public void blankArgument_DefinedBlank() throws Exception {
    var code =
        """
        foo _ =
            42
        """;
    var sig = DumpTestUtils.generateSignatures(ctxRule, code, "Main");
    assertSingleBlankArgument("foo", sig);
  }

  @Test
  public void moreBlankArguments() throws Exception {
    var code =
        """
        foo _ x _ y =
            42
        """;
    var sig = DumpTestUtils.generateSignatures(ctxRule, code, "Main");
    var sigLine = lastLine(sig);
    var any = "Standard.Base.Any.Any";
    var regex = ".*foo _:${any} x:${any} _:${any} y:${any} ->.*".replace("${any}", any);
    assertThat("Two blank (underscore) arguments: " + sigLine, sigLine.matches(regex), is(true));
  }

  @Test
  public void conversionBlank() throws Exception {
    var code =
        """
        type Source
        type Target
        Target.from (_:Source) = 42
        """;
    var sig = DumpTestUtils.generateSignatures(ctxRule, code, "Main");
    var sigLine = lastLine(sig);
    assertThat(
        "Single blank argument in conversion: " + sigLine,
        sigLine,
        containsString("Main.Target.from _:Main.Source -> Main.Target"));
  }

  private static void assertSingleBlankArgument(String methodName, String sig) {
    var sigLine = lastLine(sig);
    var regex = ".*" + methodName + " _:Standard.Base.Any.Any ->.*";
    assertThat("Single blank (underscore) argument: " + sigLine, sigLine.matches(regex), is(true));
  }

  private static String lastLine(String text) {
    var lines = text.lines().toList();
    assert !lines.isEmpty();
    return lines.get(lines.size() - 1);
  }

  private static void generateDocumentation(String projectName, String code, DocsVisit v)
      throws IOException {
    var pathCalc = TEMP.newFolder(projectName);
    DumpTestUtils.generateDocumentation(pathCalc.toPath(), projectName, code, v);
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

  record TypeAnd<IRElement>(Definition.Type t, IRElement ir) {}
}
