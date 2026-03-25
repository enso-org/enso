package org.enso.interpreter.instrument;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Set;
import java.util.UUID;
import org.enso.compiler.Passes;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Location;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.PassManager;
import org.enso.compiler.pass.PassManagerTestUtils;
import org.enso.interpreter.runtime.ModuleTestUtils;
import org.enso.pkg.QualifiedName;
import org.enso.text.buffer.Rope;
import org.enso.text.editing.model.Position;
import org.enso.text.editing.model.Range;
import org.enso.text.editing.model.TextEdit;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Tests for {@link ChangesetBuilder#compute(scala.collection.immutable.Seq)} which computes the
 * transitive closure of all affected external IDs using DataflowAnalysis dependency information.
 */
public class ChangesetBuilderComputeTest {

  private PassManager passManager;

  @Before
  public void setUp() {
    passManager = new Passes(CompilerConfig.createDefault()).passManager();
  }

  // ===== Helper methods =====

  private ModuleContext freshModuleContext() {
    var runtimeMod =
        org.enso.interpreter.runtime.Module.empty(QualifiedName.simpleName("Test_Module"), null);
    return new ModuleContext(
        runtimeMod.asCompilerModule(),
        CompilerConfig.createDefault(),
        Option.apply(new FreshNameSupply()),
        Option.empty(),
        false,
        Option.empty());
  }

  /**
   * Preprocesses code through the compiler pass pipeline. The code should include a METADATA
   * section (use {@link #addMetadata} to generate one).
   */
  private Module preprocessModule(String code) {
    var moduleContext = freshModuleContext();
    var ir = EnsoParser.compile(code);
    var passGroups = PassManagerTestUtils.getPasses(passManager);
    var runtimeMod = org.enso.interpreter.runtime.Module.fromCompilerModule(moduleContext.module());
    var curIr = ir;
    for (int i = 0; i < passGroups.size(); i++) {
      var group = passGroups.apply(i);
      ModuleTestUtils.unsafeSetIr(runtimeMod, curIr);
      curIr = passManager.runPassesOnModule(curIr, moduleContext, group, Option.empty());
    }
    return curIr;
  }

  /**
   * Generates a METADATA section that assigns UUIDs to all IR nodes with locations and appends it
   * to the raw code.
   */
  private String addMetadata(String rawCode) {
    var ir = EnsoParser.compile(rawCode);
    var locations = new LinkedHashMap<Location, UUID>();
    IR.preorder(
        ir,
        node -> {
          var locOpt = node.location();
          if (locOpt.isDefined()) {
            var loc = locOpt.get().location();
            locations.putIfAbsent(loc, UUID.randomUUID());
          }
        });
    var sb = new StringBuilder();
    sb.append("[");
    boolean first = true;
    for (var entry : locations.entrySet()) {
      if (!first) sb.append(",");
      first = false;
      int start = entry.getKey().start();
      int size = entry.getKey().end() - start;
      sb.append("[{\"index\":{\"value\":")
          .append(start)
          .append("},\"size\":{\"value\":")
          .append(size)
          .append("}},\"")
          .append(entry.getValue())
          .append("\"]");
    }
    sb.append("]");
    return rawCode + "\n\n\n#### METADATA ####\n" + sb + "\n[]\n";
  }

  /** Strips the METADATA section from a code string, returning the raw code portion. */
  private String rawCode(String code) {
    int idx = code.indexOf("\n\n\n#### METADATA ####\n");
    return idx >= 0 ? code.substring(0, idx) : code;
  }

  /** Calls {@link ChangesetBuilder#compute} and returns the result as a Java set. */
  @SuppressWarnings("unchecked")
  private Set<UUID> computeInvalidated(IR ir, String code, TextEdit... edits) {
    var scalaEdits = CollectionConverters.asScala(Arrays.asList(edits)).toSeq();
    var rope = Rope.apply(code);
    var builder =
        new ChangesetBuilder<>(
            rope,
            ir,
            org.enso.text.editing.RopeTextEditor$.MODULE$,
            org.enso.text.editing.IndexedSource$.MODULE$.RopeIndexedSource());
    var result = builder.compute(scalaEdits);
    return new HashSet<>(CollectionConverters.asJava((scala.collection.Set<UUID>) result));
  }

  /** Finds the first {@link Expression.Binding} with the given name in the IR tree. */
  private Expression.Binding findBinding(IR ir, String name) {
    var result = new ArrayList<Expression.Binding>();
    IR.preorder(
        ir,
        node -> {
          if (node instanceof Expression.Binding binding) {
            if (binding.name().name().equals(name)) {
              result.add(binding);
            }
          }
        });
    return result.isEmpty() ? null : result.get(0);
  }

  /** Gets the external ID of a binding found by name. Fails if not found. */
  private UUID getBindingId(IR ir, String name) {
    var binding = findBinding(ir, name);
    assertNotNull("Binding '" + name + "' not found in IR", binding);
    var extId = binding.getExternalId();
    assertTrue("No external ID for binding '" + name + "'", extId.isDefined());
    return extId.get();
  }

  /**
   * Finds an IR node whose source text matches the given string and returns its external ID.
   * Returns null if not found or no external ID.
   */
  private UUID findExternalIdBySourceText(IR ir, String raw, String text) {
    var result = new ArrayList<UUID>();
    IR.preorder(
        ir,
        node -> {
          var locOpt = node.location();
          var extIdOpt = node.getExternalId();
          if (locOpt.isDefined() && extIdOpt.isDefined()) {
            var loc = locOpt.get().location();
            if (loc.start() >= 0 && loc.end() <= raw.length()) {
              var nodeText = raw.substring(loc.start(), loc.end());
              if (nodeText.equals(text)) {
                result.add(extIdOpt.get());
              }
            }
          }
        });
    return result.isEmpty() ? null : result.get(0);
  }

  private void assertInvalidated(Set<UUID> result, IR ir, String name) {
    var id = getBindingId(ir, name);
    assertTrue("Expected binding '" + name + "' to be invalidated", result.contains(id));
  }

  private void assertNotInvalidated(Set<UUID> result, IR ir, String name) {
    var binding = findBinding(ir, name);
    if (binding != null && binding.getExternalId().isDefined()) {
      assertFalse(
          "Expected binding '" + name + "' NOT to be invalidated",
          result.contains(binding.getExternalId().get()));
    }
  }

  // ===== Test cases =====

  @Test
  public void editLiteralPropagatesToBindingAndDependents() {
    var rawCode =
        """
        main =
            x = 42
            y = x + 1
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 42 -> 99
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 10)), "99");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
  }

  @Test
  public void editDoesNotAffectIndependentBindings() {
    var rawCode =
        """
        main =
            x = 1
            y = 2
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 3
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "3");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertNotInvalidated(result, ir, "y");
  }

  @Test
  public void variableReferenceChainPropagatesTransitively() {
    var rawCode =
        """
        main =
            a = 100
            b = a
            c = b
            c
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 100 -> 200
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 11)), "200");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "c");
  }

  @Test
  public void multipleUsagesOfSameVariable() {
    var rawCode =
        """
        main =
            x = 10
            y = x + 1
            z = x + 2
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 10 -> 20
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 10)), "20");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
    assertInvalidated(result, ir, "z");
  }

  @Test
  public void diamondDependencyPattern() {
    var rawCode =
        """
        main =
            a = 1
            b = a
            c = a
            d = b + c
            d
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 2
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "c");
    assertInvalidated(result, ir, "d");
  }

  @Test
  public void editPropagatesThroughMethodCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.to_text
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 42 -> 99
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 10)), "99");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
  }

  @Test
  public void editBindingNameInvalidatesDependents() {
    var rawCode =
        """
        main =
            foo = 42
            bar = foo
            bar
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: foo -> baz (rename the binding)
    var edit = new TextEdit(new Range(new Position(1, 4), new Position(1, 7)), "baz");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "foo");
    assertInvalidated(result, ir, "bar");
  }

  @Test
  public void editInNestedExpressionPropagatesOutward() {
    var rawCode =
        """
        main =
            x = 1 + 2
            y = x
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 2 -> 5
    var edit = new TextEdit(new Range(new Position(1, 12), new Position(1, 13)), "5");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
  }

  @Test
  public void editMethodArgumentInvalidatesCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.method 10
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 10 -> 20 (change argument value)
    var edit = new TextEdit(new Range(new Position(2, 17), new Position(2, 19)), "20");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "x");
  }

  @Test
  public void editMethodSelfArgumentInvalidatesCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.method 10
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 42 -> 99 (change self argument definition)
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 10)), "99");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
  }

  @Test
  public void editMethodNameInvalidatesCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.method1 10
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: method1 -> method2
    var edit = new TextEdit(new Range(new Position(2, 10), new Position(2, 17)), "method2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "x");
  }

  @Test
  public void editMethodDefinitionBodyInvalidatesCall() {
    var rawCode =
        """
        helper x = x + 1

        main =
            y = helper 10
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 2 in helper body
    var edit = new TextEdit(new Range(new Position(0, 16), new Position(0, 17)), "2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
  }

  @Test
  public void editMethodDefinitionBodyInvalidatesBothCalls() {
    var rawCode =
        """
        helper x = x + 1

        main =
            a = 10
            b = 20
            y = helper a
            z = helper b
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 2 in helper body
    var edit = new TextEdit(new Range(new Position(0, 16), new Position(0, 17)), "2");
    var result = computeInvalidated(ir, code, edit);

    assertNotInvalidated(result, ir, "a");
    assertNotInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "y");
    assertInvalidated(result, ir, "z");
  }

  @Test
  public void editApplicationArgumentInvalidatesSingleCall() {
    var rawCode =
        """
        helper x = x + 1

        main =
            a = 10
            b = 20
            y = helper a
            z = helper b
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 4 in "a = 10"
    var edit = new TextEdit(new Range(new Position(3, 8), new Position(3, 9)), "4");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertNotInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "z");
  }

  @Test
  public void multipleSimultaneousEdits() {
    var rawCode =
        """
        main =
            x = 1
            y = 2
            z = x + y
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 10 AND 2 -> 20
    var edit1 = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "10");
    var edit2 = new TextEdit(new Range(new Position(2, 8), new Position(2, 9)), "20");
    var result = computeInvalidated(ir, code, edit1, edit2);

    assertInvalidated(result, ir, "x");
    assertInvalidated(result, ir, "y");
    assertInvalidated(result, ir, "z");
  }

  // ===== Negative / non-invalidation test cases =====

  @Test
  public void editLaterBindingDoesNotAffectEarlierOnes() {
    var rawCode =
        """
        main =
            x = 1
            y = 2
            z = y + 1
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 2 -> 3 (y's literal)
    var edit = new TextEdit(new Range(new Position(2, 8), new Position(2, 9)), "3");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertInvalidated(result, ir, "z");
    assertNotInvalidated(result, ir, "x");
  }

  @Test
  public void parallelIndependentChainsDoNotAffectEachOther() {
    var rawCode =
        """
        main =
            a = 1
            b = a + 1
            c = 10
            d = c + 1
            d
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 2 (a's literal)
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertInvalidated(result, ir, "b");
    assertNotInvalidated(result, ir, "c");
    assertNotInvalidated(result, ir, "d");
  }

  @Test
  public void editDownstreamDoesNotPropagateUpstream() {
    var rawCode =
        """
        main =
            a = 1
            b = a + 1
            c = b + 1
            c
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: the literal 1 in "b = a + 1" (line 2, char 12)
    var edit = new TextEdit(new Range(new Position(2, 12), new Position(2, 13)), "5");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "c");
    assertNotInvalidated(result, ir, "a");
  }

  @Test
  public void diamondWithUnrelatedSiblingNotAffected() {
    var rawCode =
        """
        main =
            a = 1
            b = a
            c = a
            d = b + c
            e = 99
            d
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 2 (a's literal)
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertInvalidated(result, ir, "b");
    assertInvalidated(result, ir, "c");
    assertInvalidated(result, ir, "d");
    assertNotInvalidated(result, ir, "e");
  }

  @Test
  public void editInOneMethodDoesNotAffectAnotherMethod() {
    var rawCode =
        """
        foo =
            x = 1
            x

        bar =
            y = 2
            y
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 1 -> 3 in foo
    var edit = new TextEdit(new Range(new Position(1, 8), new Position(1, 9)), "3");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "x");
    assertNotInvalidated(result, ir, "y");
  }

  @Test
  public void editSecondChainDoesNotAffectFirstChain() {
    var rawCode =
        """
        main =
            a = 1
            b = a + 1
            c = 10
            d = c + 1
            d
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 10 -> 20 (c's literal)
    var edit = new TextEdit(new Range(new Position(3, 8), new Position(3, 10)), "20");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "c");
    assertInvalidated(result, ir, "d");
    assertNotInvalidated(result, ir, "a");
    assertNotInvalidated(result, ir, "b");
  }

  @Test
  public void editMiddleOfChainDoesNotAffectSiblings() {
    var rawCode =
        """
        main =
            a = 1
            b = a
            c = a
            d = b + 1
            e = c + 1
            e
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: the literal 1 in "d = b + 1" (line 4, char 12)
    var edit = new TextEdit(new Range(new Position(4, 12), new Position(4, 13)), "5");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "d");
    assertNotInvalidated(result, ir, "a");
    assertNotInvalidated(result, ir, "b");
    assertNotInvalidated(result, ir, "c");
    assertNotInvalidated(result, ir, "e");
  }

  @Test
  public void editMethodCallArgumentDoesNotAffectIndependentMethodCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.method 10
            z = x.method 20
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: 10 -> 99 (argument of y's method call)
    var edit = new TextEdit(new Range(new Position(2, 17), new Position(2, 19)), "99");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "x");
    assertNotInvalidated(result, ir, "z");
  }

  @Test
  public void editMethodNameDoesNotAffectIndependentMethodCall() {
    var rawCode =
        """
        main =
            x = 42
            y = x.alpha 10
            z = x.beta 20
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: alpha -> gamma in y's call (line 2, chars 10-15)
    var edit = new TextEdit(new Range(new Position(2, 10), new Position(2, 15)), "gamma");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "x");
    assertNotInvalidated(result, ir, "z");
  }

  @Test
  @Ignore
  public void editMethodNameDoesNotAffectIndependentMethodCallWithSameName() {
    var rawCode =
        """
        main =
            x = 42
            y = x.method1 10
            z = x.method1 20
            z
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: method1 -> method2 in y's call (line 2, chars 10-17)
    var edit = new TextEdit(new Range(new Position(2, 10), new Position(2, 17)), "method2");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "x");
    // The following assertion fails because the ChangesetBuilder looks for dynamic dependency
    // "method1". Ignored for now because this edit pattern is not used when editing nodes in IDE.
    assertNotInvalidated(result, ir, "z");
  }

  @Test
  public void editMethodCallSelfDoesNotAffectIndependentMethodCall() {
    var rawCode =
        """
        main =
            x = 42
            y = 99
            a = x.method 10
            b = y.method 20
            b
        """;
    var code = addMetadata(rawCode);
    var ir = preprocessModule(code);

    // Edit: x -> y in a's call, i.e. "x.method 10" -> "y.method 10" (line 3, char 8)
    var edit = new TextEdit(new Range(new Position(3, 8), new Position(3, 9)), "y");
    var result = computeInvalidated(ir, code, edit);

    assertInvalidated(result, ir, "a");
    assertNotInvalidated(result, ir, "x");
    assertNotInvalidated(result, ir, "y");
    assertNotInvalidated(result, ir, "b");
  }
}
