package org.enso.compiler.test.ir;

import static org.enso.scala.wrapper.ScalaConversions.asJava;
import static org.enso.scala.wrapper.ScalaConversions.asScala;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.ArrayDeque;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.DefinitionArgument.Specified;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Name.Literal;
import org.enso.compiler.core.ir.Name.Qualified;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import scala.Option;

/**
 * This test suite ensures that contracts for shallow and deep copies of IR nodes are respected.
 *
 * <h2>Shallow copy</h2>
 *
 * Various copy methods, like {@link
 * org.enso.compiler.core.ir.DefinitionArgument.Specified#copy(Name, Option)} exist in {@code
 * org.enso.compiler.core.ir} package. The contract for these {@code copy} methods is that they
 * create a <i>shallow</i> copy, unlike {@link org.enso.compiler.core.IR#duplicate(boolean, boolean,
 * boolean, boolean) IR.duplicate} which creates a <i>deep</i> copy.
 *
 * <p>Java IR elements generated with {@link org.enso.runtime.parser.dsl.GenerateIR} annotation
 * processor have copy builders generated for them.
 *
 * <h2>Deep copy</h2>
 *
 * Deep copy is achieved with {@link org.enso.compiler.core.IR#duplicate(boolean, boolean, boolean,
 * boolean) IR.duplicate}.
 */
@RunWith(Parameterized.class)
public class IRShallowCopyTest {

  @Parameters(name = "{0}")
  public static List<TestInput> parameters() {
    var simpleLit =
        TestInput.withName("Simple literal")
            .originalIr(() -> IRUtils.literal("x"))
            .copyFunc(
                lit -> {
                  var l = (Literal) lit;
                  return l.copy(
                      l.name(),
                      l.isMethod(),
                      l.location(),
                      l.originalName(),
                      l.passData(),
                      l.diagnostics(),
                      l.id());
                })
            .build();

    var simpleDefArg =
        TestInput.withName("Simple DefinitionArgument")
            .originalIr(
                () -> {
                  var lit = IRUtils.literal("x");
                  return defArgBuilder().name(lit).build();
                })
            .copyFunc(defArg -> defArgCopyBuilder((Specified) defArg).build())
            .build();

    var defArgWithName =
        TestInput.withName("DefinitionArgument with name and default value")
            .originalIr(
                () -> {
                  var name = IRUtils.literal("name");
                  var defaultValue = IRUtils.emptyIr();
                  return defArgBuilder()
                      .defaultValue(Option.apply(defaultValue))
                      .name(name)
                      .build();
                })
            .copyFunc(defArg -> defArgCopyBuilder((Specified) defArg).build())
            .build();

    var qualifiedName =
        TestInput.withName("QualifiedName")
            .originalIr(
                () -> {
                  var name = IRUtils.literal("x");
                  var qualified =
                      new Qualified(asScala(List.of(name)), null, new MetadataStorage());
                  return qualified;
                })
            .copyFunc(
                qualified -> {
                  var q = (Qualified) qualified;
                  return q.copy(q.parts(), Option.empty(), q.passData(), null, null);
                })
            .build();

    var caseExprInput =
        TestInput.withName("Case.Expr")
            .originalIr(
                () -> {
                  var pat =
                      new Pattern.Literal(
                          new org.enso.compiler.core.ir.Literal.Text(
                              "foo", null, new MetadataStorage()),
                          null,
                          new MetadataStorage());
                  var branch =
                      Case.Branch.builder()
                          .terminalBranch(true)
                          .expression(IRUtils.emptyIr())
                          .location(new IdentifiedLocation(1, 10, null))
                          .pattern(pat)
                          .build();
                  return Case.Expr.builder()
                      .scrutinee(IRUtils.emptyIr())
                      .branches(asScala(List.of(branch)))
                      .isNested(false)
                      .build();
                })
            .copyFunc(
                (ir) -> {
                  var caseExpr = (Case.Expr) ir;
                  return Case.Expr.builder(caseExpr).build();
                })
            .build();

    return List.of(simpleLit, simpleDefArg, defArgWithName, qualifiedName, caseExprInput);
  }

  private final TestInput testInput;

  public IRShallowCopyTest(TestInput testInput) {
    this.testInput = testInput;
  }

  @Test
  public void testShallowCopy() {
    var ir = testInput.originalIr.get();
    var copy = testInput.copyFunc.apply(ir);
    assertPassDataAreSame(ir, copy);
  }

  private static DefinitionArgument.Specified.Builder defArgBuilder() {
    return DefinitionArgument.Specified.builder()
        .defaultValue(Option.empty())
        .ascribedType(Option.empty());
  }

  private static DefinitionArgument.Specified.Builder defArgCopyBuilder(
      DefinitionArgument.Specified obj) {
    return DefinitionArgument.Specified.builder(obj);
  }

  private static void assertPassDataAreSame(IR original, IR copy) {
    var originalStack = new ArrayDeque<IR>();
    var copyStack = new ArrayDeque<IR>();
    originalStack.addLast(original);
    copyStack.addLast(copy);
    var processingRoot = true;
    while (!originalStack.isEmpty()) {
      var curOriginal = originalStack.removeLast();
      var curCopy = copyStack.removeLast();

      assertSameTypes(curOriginal, curCopy);
      assertSameMeta(curOriginal, curCopy);
      assertSameDiagnostics(curOriginal, curCopy);

      var curOriginalChildren = asJava(curOriginal.children());
      var curCopyChildren = asJava(curCopy.children());
      assertThat(
          "IR copy produced different AST", curOriginalChildren.size(), is(curCopyChildren.size()));
      if (!processingRoot) {
        assertSameLocation(curOriginal, curCopy);

        for (int i = 0; i < curOriginalChildren.size(); i++) {
          var origChild = curOriginalChildren.get(i);
          var copyChild = curCopyChildren.get(i);
          assertThat("Nested children should be same references", origChild == copyChild, is(true));
        }
      }
      originalStack.addAll(curOriginalChildren);
      copyStack.addAll(curCopyChildren);
      processingRoot = false;
    }
  }

  private static void assertSameTypes(IR original, IR copy) {
    if (!original.getClass().getName().equals(copy.getClass().getName())) {
      throw new AssertionError(
          "IR copy has different type than original: "
              + original.getClass().getName()
              + " != "
              + copy.getClass().getName());
    }
  }

  private static void assertSameDiagnostics(IR original, IR copy) {
    var curOriginalDiag = original.diagnostics();
    var curCopyDiag = copy.diagnostics();
    assertThat(
        "IR copy has different diagnostics than original",
        curOriginalDiag == curCopyDiag,
        is(true));
  }

  private static void assertSameMeta(IR original, IR copy) {
    var curOriginalMeta = original.passData();
    var curCopyMeta = copy.passData();
    assertThat(
        "IR copy has different passData than original: "
            + "original: "
            + original
            + ", copy: "
            + copy,
        curOriginalMeta == curCopyMeta,
        is(true));
  }

  private static void assertSameLocation(IR original, IR copy) {
    var origLoc = original.location();
    var copyLoc = copy.location();
    if (origLoc.isDefined() && copyLoc.isDefined()) {
      assertThat("IR copy has different location than original", origLoc.get(), is(copyLoc.get()));
    } else {
      assertThat("Both are empty", origLoc.isEmpty(), is(true));
      assertThat("Both are empty", copyLoc.isEmpty(), is(true));
    }
  }

  public static final class TestInput {
    final String name;
    final Supplier<IR> originalIr;
    final Function<IR, IR> copyFunc;

    private TestInput(String name, Supplier<IR> originalIr, Function<IR, IR> copyFunc) {
      this.name = name;
      this.originalIr = originalIr;
      this.copyFunc = copyFunc;
    }

    static Builder withName(String name) {
      return new Builder(name);
    }

    @Override
    public String toString() {
      return name;
    }

    private static final class Builder {
      private final String name;
      private Supplier<IR> originalIr;
      private Function<IR, IR> copyFunc;

      private Builder(String name) {
        this.name = name;
      }

      Builder originalIr(Supplier<IR> originalIr) {
        this.originalIr = originalIr;
        return this;
      }

      Builder copyFunc(Function<IR, IR> copyFunc) {
        this.copyFunc = copyFunc;
        return this;
      }

      TestInput build() {
        if (originalIr == null) {
          throw new IllegalStateException("originalIr must be set");
        }
        if (copyFunc == null) {
          throw new IllegalStateException("copyFunc must be set");
        }
        return new TestInput(name, originalIr, copyFunc);
      }
    }
  }
}
