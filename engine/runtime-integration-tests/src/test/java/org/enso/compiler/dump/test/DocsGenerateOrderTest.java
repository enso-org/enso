package org.enso.compiler.dump.test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Definition.Data;
import org.enso.compiler.core.ir.module.scope.Definition.Type;
import org.enso.compiler.core.ir.module.scope.definition.Method.Conversion;
import org.enso.compiler.core.ir.module.scope.definition.Method.Explicit;
import org.enso.compiler.docs.DocsVisit;
import org.enso.pkg.QualifiedName;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

/**
 * IR elements in a module should be visited in a specific order. This test checks that the order is
 * correct.
 */
public class DocsGenerateOrderTest {
  @ClassRule public static final TemporaryFolder TMP_DIR = new TemporaryFolder();

  @Test
  public void moduleElementsAreVisitedInCorrectOrder() throws IOException {
    var projName = "Proj";
    var moduleName = "local." + projName + ".Main";
    var aTypeName = "A_Type";
    var bTypeName = "B_Type";
    var src =
        """
        import Standard.Base.Any.Any

        b_module_method = 1             # 7

        type B_Type                     # 5

        Any.extension_method self = 3   # 8

        type A_Type                     # 0
            B_Cons                      # 2
            A_Cons                      # 1
            b_method self = 1           # 4
            a_method self = 2           # 3

        B_Type.from _:A_Type = 4        # 9
        a_module_method = 2             # 6
        """;
    var projDir = TMP_DIR.newFolder();
    var timestampVisitor = new TimestampVisitor();
    DumpTestUtils.generateDocumentation(projDir.toPath(), "Proj", src, timestampVisitor);
    var events = timestampVisitor.events;
    List<Event> expectedEvents =
        List.of(
            new VisitedModule(moduleName),
            new VisitedType(aTypeName),
            new VisitedConstructor(aTypeName, "A_Cons"),
            new VisitedConstructor(aTypeName, "B_Cons"),
            new VisitedMethod(aTypeName, "a_method"),
            new VisitedMethod(aTypeName, "b_method"),
            new VisitedType(bTypeName),
            new VisitedMethod(null, "a_module_method"),
            new VisitedMethod(null, "b_module_method"),
            new VisitedMethod(null, "extension_method"),
            new VisitedConversion(bTypeName, aTypeName));
    var expectedEventsArr = expectedEvents.toArray(Event[]::new);
    assertThat(events, contains(expectedEventsArr));
  }

  private static final class TimestampVisitor implements DocsVisit {
    private final List<Event> events = new ArrayList<>();

    @Override
    public boolean visitModule(QualifiedName name, Module ir, PrintWriter writer) {
      events.add(new VisitedModule(name.toString()));
      return true;
    }

    @Override
    public boolean visitUnknown(IR ir, PrintWriter w) {
      return true;
    }

    @Override
    public void visitMethod(Type t, Explicit m, PrintWriter writer) {
      var typeName = t == null ? null : t.name().name();
      events.add(new VisitedMethod(typeName, m.methodName().name()));
    }

    @Override
    public void visitConversion(Conversion c, PrintWriter w) {
      var targetTypeName = c.typeName().get().name();
      var sourceTypeName = ((Name.Literal) c.sourceTypeName()).name();
      events.add(new VisitedConversion(targetTypeName, sourceTypeName));
    }

    @Override
    public boolean visitType(Type t, PrintWriter w) {
      events.add(new VisitedType(t.name().name()));
      return true;
    }

    @Override
    public void visitConstructor(Type t, Data d, PrintWriter w) {
      events.add(new VisitedConstructor(t.name().name(), d.name().name()));
    }
  }

  /** All names are unqualified */
  private interface Event {}

  private record VisitedModule(String moduleName) implements Event {}

  /**
   * @param typeName Can be null if this is a module method
   * @param methodName
   */
  private record VisitedMethod(String typeName, String methodName) implements Event {}

  private record VisitedConversion(String targetTypeName, String sourceTypeName) implements Event {}

  private record VisitedType(String typeName) implements Event {}

  private record VisitedConstructor(String typeName, String consName) implements Event {}
}
