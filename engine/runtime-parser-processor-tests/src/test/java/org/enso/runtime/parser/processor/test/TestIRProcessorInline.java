package org.enso.runtime.parser.processor.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;

import com.google.testing.compile.Compilation;
import com.google.testing.compile.CompilationSubject;
import com.google.testing.compile.Compiler;
import com.google.testing.compile.JavaFileObjects;
import java.io.IOException;
import org.enso.runtime.parser.processor.IRProcessor;
import org.junit.Test;

/**
 * Basic tests of {@link IRProcessor} that compiles snippets of annotated code, and checks the
 * generated classes. The compiler (along with the processor) is invoked in the unit tests.
 */
public class TestIRProcessorInline {
  /**
   * Compiles the code given in {@code src} with {@link IRProcessor} and returns the contents of the
   * generated java source file.
   *
   * @param name FQN of the Java source file
   * @param src
   * @return
   */
  private static String generatedClass(String name, String src) {
    var srcObject = JavaFileObjects.forSourceString(name, src);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(srcObject);
    CompilationSubject.assertThat(compilation).succeeded();
    assertThat("Generated just one source", compilation.generatedSourceFiles().size(), is(1));
    var generatedSrc = compilation.generatedSourceFiles().get(0);
    try {
      return generatedSrc.getCharContent(false).toString();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  private static void expectCompilationFailure(String src) {
    var srcObject = JavaFileObjects.forSourceString("TestHello", src);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(srcObject);
    CompilationSubject.assertThat(compilation).failed();
  }

  private static Compilation compile(String name, String src) {
    var srcObject = JavaFileObjects.forSourceString(name, src);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(srcObject);
    return compilation;
  }

  @Test
  public void simpleIRNodeWithoutFields_CompilationSucceeds() {
    var src =
        JavaFileObjects.forSourceString(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void onlyFinalClassCanBeAnnotated() {
    var src =
        JavaFileObjects.forSourceString(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        @GenerateIR
        public class JName {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).failed();
    CompilationSubject.assertThat(compilation).hadErrorCount(1);
    CompilationSubject.assertThat(compilation).hadErrorContaining("final");
  }

  @Test
  public void annotatedClass_MustHaveAnnotatedConstructor() {
    var src =
        JavaFileObjects.forSourceString(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        @GenerateIR
        public final class JName {}
        """);
    var compiler = Compiler.javac().withProcessors(new IRProcessor());
    var compilation = compiler.compile(src);
    CompilationSubject.assertThat(compilation).failed();
    CompilationSubject.assertThat(compilation)
        .hadErrorContaining("must have exactly one constructor annotated with @GenerateFields");
  }

  @Test
  public void annotatedClass_MustExtendGeneratedSuperclass() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName {
          @GenerateFields
          public JName() {}
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).failed();
    CompilationSubject.assertThat(compilation).hadErrorContaining("must have 'extends' clause");
  }

  @Test
  public void annotatedClass_InterfacesToImplement_CanHaveMore() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.compiler.core.IR;

        interface MySuperIR { }

        @GenerateIR(interfaces = {MySuperIR.class, IR.class})
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR() {}
        }
  """;
    var generatedClass = generatedClass("MyIR", src);
    assertThat(generatedClass, containsString("class MyIRGen implements IR, MySuperIR"));
  }

  @Test
  public void annotatedClass_InterfacesToImplement_DoNotHaveToExtendIR() {
    var src =
        """
    import org.enso.runtime.parser.dsl.GenerateIR;
    import org.enso.runtime.parser.dsl.GenerateFields;

    interface MySuperIR_1 { }
    interface MySuperIR_2 { }

    @GenerateIR(interfaces = {MySuperIR_1.class, MySuperIR_2.class})
    public final class MyIR extends MyIRGen {
      @GenerateFields
      public MyIR() {}
    }
""";
    var generatedClass = generatedClass("MyIR", src);
    assertThat(
        generatedClass, containsString("class MyIRGen implements IR, MySuperIR_1, MySuperIR_2"));
  }

  @Test
  public void simpleIRNodeWithUserDefinedFiled_CompilationSucceeds() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(@IRField String name) {
            super(name);
          }
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("class JNameGen"));
    assertThat("Getter for 'name' generated", genClass, containsString("String name()"));
  }

  @Test
  public void generatedClassHasProtectedConstructor() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("class JNameGen"));
    assertThat(
        "Generate class has protected constructor", genClass, containsString("protected JNameGen"));
  }

  /**
   * The default generated protected constructor has the same signature as the constructor in
   * subtype annotated with {@link org.enso.runtime.parser.dsl.GenerateFields}
   */
  @Test
  public void generatedClassHasConstructorMatchingSubtype_Empty() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {
            super();
          }
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void generatedClassHasConstructorMatchingSubtype_UserFields() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(@IRField boolean suspended, @IRField String name) {
            super(suspended, name);
          }
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void generatedClassHasConstructorMatchingSubtype_MetaFields() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.compiler.core.ir.DiagnosticStorage;
        import org.enso.compiler.core.ir.IdentifiedLocation;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(DiagnosticStorage diag, IdentifiedLocation loc) {
            super(diag, loc);
          }
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void generatedClassHasConstructorMatchingSubtype_UserFieldsAndMetaFields() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;
        import org.enso.compiler.core.ir.DiagnosticStorage;
        import org.enso.compiler.core.ir.IdentifiedLocation;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(DiagnosticStorage diag, IdentifiedLocation loc, @IRField boolean suspended) {
            super(diag, loc, suspended);
          }
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void generatedClass_IsAbstract() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("abstract class JNameGen"));
  }

  @Test
  public void generatedClass_CanHaveArbitraryName() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends MySuperGeneratedClass {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("abstract class MySuperGeneratedClass"));
  }

  /**
   * Generated {@code duplicate} method returns the annotated class type, not any of its super
   * types.
   */
  @Test
  public void generatedClass_DuplicateMethodHasSpecificReturnType() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("JName duplicate("));
  }

  /** Parameterless {@code duplicate} method just delegates to the other duplicate method. */
  @Test
  public void generatedClass_HasParameterlessDuplicateMethod() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("JName duplicate()"));
  }

  @Test
  public void generatedMethod_setLocation_returnsSubClassType() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("JName setLocation("));
  }

  @Test
  public void generatedMethod_mapExpressions_returnsSubClassType() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName() {}
        }
        """;
    var genClass = generatedClass("JName", src);
    assertThat(genClass, containsString("JName mapExpressions("));
  }

  @Test
  public void annotatedConstructor_MustNotHaveUnannotatedParameters() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(int param) {}
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).failed();
    CompilationSubject.assertThat(compilation).hadErrorContaining("must be annotated");
  }

  @Test
  public void annotatedConstructor_CanHaveMetaParameters() {
    var src =
        """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.compiler.core.ir.MetadataStorage;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(MetadataStorage passData) {
            super(passData);
          }
        }
        """;
    var compilation = compile("JName", src);
    CompilationSubject.assertThat(compilation).succeeded();
  }

  @Test
  public void simpleIRNodeWithChild() {
    var genSrc =
        generatedClass(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRChild;
        import org.enso.compiler.core.ir.Expression;

        @GenerateIR
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR(@IRChild Expression expression) {
            super(expression);
          }
        }
        """);
    assertThat(genSrc, containsString("Expression expression()"));
  }

  @Test
  public void irNodeWithMultipleFields_PrimitiveField() {
    var genSrc =
        generatedClass(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRChild;
        import org.enso.runtime.parser.dsl.IRField;

        @GenerateIR
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR(@IRField boolean suspended) {
            super(suspended);
          }
        }
        """);
    assertThat(genSrc, containsString("boolean suspended()"));
  }

  @Test
  public void irNodeWithInheritedField() {
    var src =
        generatedClass(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;
        import org.enso.compiler.core.IR;

        interface MySuperIR extends IR {
          boolean suspended();
        }

        @GenerateIR(interfaces = {MySuperIR.class})
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR(@IRField boolean suspended) {
            super(suspended);
          }
        }
        """);
    assertThat(src, containsString("boolean suspended()"));
  }

  @Test
  public void irNodeWithInheritedField_Override() {
    var src =
        generatedClass(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;
        import org.enso.compiler.core.IR;

        interface MySuperIR extends IR {
          boolean suspended();
        }

        @GenerateIR
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR(@IRField boolean suspended) {
            super(suspended);
          }
        }

        """);
    assertThat(src, containsString("boolean suspended()"));
  }

  @Test
  public void irNodeWithInheritedField_Transitive() {
    var src =
        generatedClass(
            "MyIR",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;
        import org.enso.compiler.core.IR;

        interface MySuperSuperIR extends IR {
          boolean suspended();
        }

        interface MySuperIR extends MySuperSuperIR {
        }

        @GenerateIR(interfaces = {MySuperIR.class})
        public final class MyIR extends MyIRGen {
          @GenerateFields
          public MyIR(@IRField boolean suspended) {
            super(suspended);
          }
        }
        """);
    assertThat(src, containsString("boolean suspended()"));
  }

  @Test
  public void irNodeAsNestedClass() {
    var src =
        generatedClass(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRField;
        import org.enso.compiler.core.IR;

        public interface JName extends IR {
          String name();

          @GenerateIR(interfaces = {JName.class})
          public final class JBlank extends JBlankGen {
            @GenerateFields
            public JBlank(@IRField String name) {
              super(name);
            }
          }
        }
        """);
    assertThat(src, containsString("class JBlankGen implements IR, JName"));
    assertThat(src, containsString("String name()"));
  }

  @Test
  public void fieldCanBeScalaList() {
    var src =
        generatedClass(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRChild;
        import org.enso.compiler.core.IR;
        import scala.collection.immutable.List;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(@IRChild List<IR> expressions) {
            super(expressions);
          }
        }
        """);
    assertThat(src, containsString("class JNameGen"));
    assertThat(src, containsString("List<IR> expressions"));
  }

  @Test
  public void fieldCanBeScalaOption() {
    var src =
        generatedClass(
            "JName",
            """
        import org.enso.runtime.parser.dsl.GenerateIR;
        import org.enso.runtime.parser.dsl.GenerateFields;
        import org.enso.runtime.parser.dsl.IRChild;
        import org.enso.compiler.core.IR;
        import scala.Option;

        @GenerateIR
        public final class JName extends JNameGen {
          @GenerateFields
          public JName(@IRChild Option<IR> expression) {
            super(expression);
          }
        }
        """);
    assertThat(src, containsString("class JNameGen"));
    assertThat("has getter method for expression", src, containsString("Option<IR> expression()"));
  }
}
