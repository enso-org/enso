package org.enso.compiler;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedType;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;
import org.junit.runners.model.Statement;
import scala.jdk.javaapi.CollectionConverters;

/**
 * If a symbol is importable by an import statement, then it should also be accessible via FQN
 * without any import (technically, with just an import of the library). This rule is enforced by
 * this test. It uses symbols from standard libraries. It scans their Main modules for exported
 * symbols, and then tests whether this invariant holds for all the symbols. Note that not all the
 * symbols are tested. For example the builtin types are not tested.
 */
@RunWith(Parameterized.class)
public class ImportsAndFQNConsistencyTest {
  private static Context ctx;

  /** Used for description in {@link PrintCodeRule} test rule. */
  private static String code;

  @Rule public final TestRule printCodeRule = new PrintCodeRule();

  /**
   * Gather all the exported symbols from Standard.Base library. Exclude builtin symbols, and
   * modules with extension methods.
   */
  @Parameters(name = "exported symbol '{0}'")
  public static List<Symbol> symbolsToTest() {
    try (var ctx =
        ContextUtils.defaultContextBuilder(LanguageInfo.ID)
            .option(RuntimeOptions.DISABLE_IR_CACHES, "false")
            .build()) {
      var ensoCtx = ContextUtils.leakContext(ctx);
      var src = """
from Standard.Base import all
from Standard.Table import all
main = 42
""";
      // Ensure that the context is initialized first.
      var res = ContextUtils.evalModule(ctx, src);
      assertThat(res.isNumber(), is(true));
      List<Symbol> symbolsToTest = new ArrayList<>();
      gatherExportedSymbols(ensoCtx, List.of("Standard.Base.Main", "Standard.Table.Main")).stream()
          .map(Symbol::new)
          .forEach(
              exportedSymbol -> {
                var mod = ensoCtx.findModule(exportedSymbol.getModuleName());
                if (mod.isPresent()) {
                  var builtin =
                      ensoCtx.getBuiltins().getBuiltinType(exportedSymbol.getLastPathItem());
                  if (builtin == null) {
                    if (mod.get().isSynthetic()) {
                      symbolsToTest.add(exportedSymbol);
                      return;
                    }
                    // The symbol is not a builtin type
                    var modIr = mod.get().getIr();
                    if (modIr != null) {
                      var bindings = mod.get().getIr().bindings();
                      if (shouldIncludeSymbolForTest(bindings, exportedSymbol.getLastPathItem())) {
                        symbolsToTest.add(exportedSymbol);
                      }
                    }
                  }
                }
              });
      return symbolsToTest;
    }
  }

  @BeforeClass
  public static void initCtx() {
    ctx = ContextUtils.createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
    ctx = null;
  }

  private final Symbol symbol;

  public ImportsAndFQNConsistencyTest(Symbol symbol) {
    this.symbol = symbol;
  }

  private void evalCode(Symbol symbol) {
    var res = ContextUtils.evalModule(ctx, code);
    assertThat(res.isString(), is(true));
    assertThat(res.asString(), is(symbol.getLastPathItem()));
  }

  /** Tests that a symbol can be used via its simple name with an FQN import of that symbol. */
  @Test
  public void testSymbolCanBeAccessedBySimpleNameWithFQNImport() {
    var sb = new StringBuilder();
    sb.append("import ").append(symbol.getFqn()).append(System.lineSeparator());
    sb.append("main = ")
        .append(symbol.getLastPathItem())
        .append(".to_text")
        .append(System.lineSeparator());
    code = sb.toString();
    evalCode(symbol);
  }

  /** Tests that a symbol can be used via FQN with an import of the library. */
  @Test
  public void testSymbolCanBeAccessedByFQNWithLibImport() {
    var sb = new StringBuilder();
    sb.append("import ").append(symbol.getLibName()).append(System.lineSeparator());
    sb.append("main = ").append(symbol.getFqn()).append(".to_text").append(System.lineSeparator());
    code = sb.toString();
    evalCode(symbol);
  }

  public static class Symbol {
    private final String fqn;
    private final List<String> pathItems;

    private Symbol(String fqn) {
      this.fqn = fqn;
      this.pathItems = Arrays.stream(fqn.split("\\.")).collect(Collectors.toList());
      assertThat(pathItems, hasSize(greaterThan(2)));
    }

    String getFqn() {
      return fqn;
    }

    String getLibName() {
      return pathItems.get(0) + "." + pathItems.get(1);
    }

    String getLastPathItem() {
      return pathItems.get(pathItems.size() - 1);
    }

    String getPathParts(int startIdx, int endIdx) {
      assert startIdx >= 0 && startIdx < endIdx && endIdx <= pathItems.size();
      return String.join(".", pathItems.subList(startIdx, endIdx));
    }

    String getPathParts(int startIdx) {
      return getPathParts(startIdx, pathItems.size());
    }

    /** Returns FQN of the module, without the last part, with library prefix. */
    String getModuleName() {
      return String.join(".", pathItems.subList(0, pathItems.size() - 1));
    }

    /** Returns qualified typename, without the library prefix. */
    String getQualifiedTypeName() {
      return String.join(".", pathItems.subList(2, pathItems.size()));
    }

    @Override
    public String toString() {
      return fqn;
    }
  }

  /**
   * A simple JUnit rule that attaches the evaluated Enso code that failed into the {@link
   * AssertionError}.
   */
  public static final class PrintCodeRule implements TestRule {
    @Override
    public Statement apply(Statement base, Description description) {
      return new Statement() {
        @Override
        public void evaluate() {
          try {
            base.evaluate();
          } catch (Throwable e) {
            var sb = new StringBuilder();
            sb.append("Test failed executing code:").append(System.lineSeparator());
            sb.append(code).append(System.lineSeparator());
            throw new AssertionError(sb.toString(), e);
          }
        }
      };
    }
  }

  private static List<String> gatherExportedSymbols(EnsoContext ensoCtx, List<String> moduleNames) {
    List<String> allExportedSymbols = new ArrayList<>();
    for (var moduleName : moduleNames) {
      allExportedSymbols.addAll(gatherExportedSymbols(moduleName, ensoCtx));
    }
    return allExportedSymbols;
  }

  private static List<String> gatherExportedSymbols(String moduleName, EnsoContext ensoCtx) {
    var mod = ensoCtx.getPackageRepository().getLoadedModule(moduleName);
    assertThat(mod.isDefined(), is(true));
    var bmExpSymbols = CollectionConverters.asJava(mod.get().getBindingsMap().exportedSymbols());
    List<String> exportedSymbols = new ArrayList<>();
    for (var entry : bmExpSymbols.entrySet()) {
      var resolvedNames = entry.getValue();
      // We are not interested in exported symbols that resolve to multiple targets.
      // We are interested only in ResolvedModule, ResolvedType, and ResolvedConstructor.
      if (resolvedNames.size() == 1) {
        var exportSymTarget = resolvedNames.apply(0);
        if (exportSymTarget instanceof ResolvedType
            || exportSymTarget instanceof ResolvedModule
            || exportSymTarget instanceof ResolvedConstructor) {
          exportedSymbols.add(exportSymTarget.qualifiedName().toString());
        }
      }
    }
    return exportedSymbols;
  }

  private static boolean shouldIncludeSymbolForTest(
      scala.collection.immutable.List<Definition> bindings, String symbol) {
    return bindings.exists(
        binding ->
            switch (binding) {
              case Definition.Type tp -> tp.name().name().equals(symbol);
              case Method.Binding methodBind -> methodBind.methodName().name().equals(symbol);
              case Method.Explicit methodExplicit -> methodExplicit
                  .methodName()
                  .name()
                  .equals(symbol);
              default -> false;
            });
  }
}
