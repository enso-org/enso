package org.enso.compiler.test.mock;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.function.Function;
import org.enso.compiler.Compiler;
import org.enso.compiler.CompilerResult;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.compiler.data.CompilerConfig;
import org.enso.logging.service.logback.MemoryAppender;
import org.enso.pkg.QualifiedName;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import org.slf4j.LoggerFactory;
import scala.jdk.javaapi.CollectionConverters;

/**
 * A {@link TestRule} that uses an alternative implementation of {@link
 * org.enso.compiler.context.CompilerContext} that holds all the information about modules in
 * memory. It is not able to resolve any standard libraries. This class only sees the modules
 * created with {@link #createModule(QualifiedName, String)}.
 *
 * <p>I recommend to use it as a {@link org.junit.Rule}, instead of {@link org.junit.ClassRule} to
 * ensure that the whole context is reset before each test.
 *
 * <h2>Usage </h2>
 *
 * To use it, {@link #createModule(QualifiedName, String) create} the necessary amount of modules.
 * If multiple projects (libraries, packages) are needed, ensure that the qualified names given to
 * the {@link #createModule(QualifiedName, String)} method contain the required library names. For
 * example:
 *
 * <pre>
 *   compilerCtx.createModule(QualifiedName.fromString("local.Proj.Main", "main = 1"));
 *   compilerCtx.createModule(QualifiedName.fromString("local.Other_Proj.Main", "main = 2"));
 * </pre>
 *
 * will create two projects with one module each.
 *
 * <p>After modules are created, the {@link #getCompiler() compiler} can be used to compile them.
 * The {@link CompilerResult#compiledModules() compiled IRs} can be inspected afterwards.
 */
public final class WithCompilerContext implements TestRule {
  private final MockPackageRepository repo;
  private final ByteArrayOutputStream out;
  private final MockCompilerContext compilerContext;
  private final Compiler compiler;

  private WithCompilerContext(ByteArrayOutputStream out, CompilerConfig compilerCfg) {
    this.repo = MockPackageRepository.create();
    this.out = out;
    this.compilerContext = new MockCompilerContext(repo, new PrintStream(out));
    this.compiler = new Compiler(compilerContext, repo, compilerCfg);
  }

  public static WithCompilerContext createDefault() {
    return new Builder().build();
  }

  public static Builder newBuilder() {
    return new Builder();
  }

  public Compiler getCompiler() {
    return compiler;
  }

  /**
   * Creates a single module with the given name and content in the virtual file system. If the
   * package that the module is contained in does not exist, it will be created. If the module with
   * the name already exists, an {@link IllegalArgumentException} is thrown.
   *
   * @param modName Qualified name of the module.
   * @param content Content of the module.
   * @return Created module. Not null.
   */
  public Module createModule(QualifiedName modName, String content) {
    return repo.createModule(modName, content);
  }

  public List<Module> getLoadedModules() {
    return CollectionConverters.asJava(repo.getLoadedModules());
  }

  /**
   * Finds the module with the given qualified name. Returns null if not found.
   *
   * @param modName Name of the module to find.
   * @return Module with the given name, or null if not found.
   */
  public Module findModule(QualifiedName modName) {
    return getLoadedModules().stream()
        .filter(mod -> mod.getName().equals(modName))
        .findFirst()
        .orElse(null);
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return new CustomStatement(base);
  }

  public final class CustomStatement extends Statement {
    private final Statement base;

    CustomStatement(Statement base) {
      this.base = base;
    }

    @Override
    public void evaluate() throws Throwable {
      try {
        base.evaluate();
      } catch (Throwable e) {
        printCompilerOutput();
        printLogsFromMemoryAppender();
        printAllVfsFiles();
        throw e;
      } finally {
        repo.getVfs().deleteAll();
        resetMemoryAppender();
      }
    }

    private void printCompilerOutput() {
      System.err.println("======== Compiler Output =========");
      System.err.println(out);
      System.err.println("===== End of Compiler Output =====");
    }

    private void printAllVfsFiles() {
      var allFiles = repo.listAllFilesInVfs();
      System.err.println("===== All files in VFS ===== ");
      System.err.println(allFiles);
      System.err.println("======== End of VFS ======== ");
    }

    private void printLogsFromMemoryAppender() {
      var appender = getMemoryAppender();
      var msgs = appender.getEvents().stream().map(CustomStatement::logEventToString).toList();
      System.err.println("======== Logs from the memory appender ========= ");
      msgs.forEach(System.err::println);
      System.err.println("===== End of logs from the memory appender ===== ");
    }

    private static void resetMemoryAppender() {
      var appender = getMemoryAppender();
      appender.reset();
    }

    private static MemoryAppender getMemoryAppender() {
      var context = (LoggerContext) LoggerFactory.getILoggerFactory();
      var logger = context.getLogger(Logger.ROOT_LOGGER_NAME);
      var appender = (MemoryAppender) logger.getAppender("memory");
      assert appender != null : "memory appender should be defined. Check application-test.conf";
      return appender;
    }

    /**
     * Memory appender does not do any kind of pattern encoding by default, so we need to do a bit
     * of formatting manually.
     */
    private static String logEventToString(ILoggingEvent logEvent) {
      return "["
          + logEvent.getLevel()
          + "] "
          + "["
          + logEvent.getLoggerName()
          + "] "
          + logEvent.getFormattedMessage();
    }
  }

  public static final class Builder {
    private CompilerConfig.Builder compilerConfigBldr = CompilerConfig.builder();

    Builder() {}

    public Builder withModifiedCompilerConfig(
        Function<CompilerConfig.Builder, CompilerConfig.Builder> cfgFunc) {
      compilerConfigBldr = cfgFunc.apply(compilerConfigBldr);
      return this;
    }

    public WithCompilerContext build() {
      var out = new ByteArrayOutputStream();
      var compilerCfg =
          compilerConfigBldr
              .autoParallelismEnabled(true)
              .warningsEnabled(true)
              .isStrictErrors(true)
              .isLintingDisabled(false)
              .outputRedirect(scala.Some.apply(new PrintStream(out)))
              .build();
      return new WithCompilerContext(out, compilerCfg);
    }
  }
}
