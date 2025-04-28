package org.enso.compiler.test.mock;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import org.apache.commons.vfs2.FileObject;
import org.enso.compiler.Compiler;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.compiler.data.CompilerConfig;
import org.enso.editions.LibraryName;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;
import scala.jdk.javaapi.CollectionConverters;

/**
 * A {@link TestRule} to use with the {@link MockCompilerContext}.
 *
 * <p>I recommend to use it as a {@link org.junit.Rule}, instead of {@link org.junit.ClassRule} to
 * ensure that the whole context is reset before each test.
 */
public final class WithMockCompilerContext implements TestRule {
  private final MockPackageRepository repo;
  private final ByteArrayOutputStream out;
  private final CompilerConfig compilerCfg;
  private final MockCompilerContext compilerContext;
  private final Compiler compiler;

  private WithMockCompilerContext(ByteArrayOutputStream out, CompilerConfig compilerCfg) {
    this.repo = MockPackageRepository.create();
    this.out = out;
    this.compilerCfg = compilerCfg;
    this.compilerContext = new MockCompilerContext(repo, new PrintStream(out));
    this.compiler = new Compiler(compilerContext, repo, compilerCfg);
  }

  public static WithMockCompilerContext createDefault() {
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

  public void registerMainProjectPackage(LibraryName pkgName, Package<FileObject> mainPkg) {
    repo.registerMainProjectPackage(pkgName, MockPackageRepository.castVirtualPkg(mainPkg));
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
        printAllVfsFiles();
        throw e;
      } finally {
        repo.getVfs().deleteAll();
      }
    }

    private void printCompilerOutput() {
      System.err.println("=== Compiler Output ===");
      System.err.println(out);
      System.err.println("=== End of Compiler Output ===");
    }

    private void printAllVfsFiles() {
      var allFiles = repo.listAllFilesInVfs();
      System.err.println("=== All files in VFS === ");
      System.err.println(allFiles);
      System.err.println("=== End of VFS === ");
    }
  }

  public static final class Builder {
    private boolean enableWarnings = true;
    private boolean enableStaticTypeInference = false;
    private boolean isStringErrors = true;
    private boolean enableLinting = true;

    Builder() {}

    public Builder enableWarnings(boolean enable) {
      this.enableWarnings = enable;
      return this;
    }

    public Builder enableStaticTypeInference(boolean enable) {
      this.enableStaticTypeInference = enable;
      return this;
    }

    public Builder isStringErrors(boolean enable) {
      this.isStringErrors = enable;
      return this;
    }

    public Builder enableLinting(boolean enable) {
      this.enableLinting = enable;
      return this;
    }

    public WithMockCompilerContext build() {
      var out = new ByteArrayOutputStream();
      var compilerCfg =
          new CompilerConfig(
              true,
              enableWarnings,
              true,
              enableStaticTypeInference,
              scala.Option.empty(),
              isStringErrors,
              enableLinting,
              scala.Some.apply(new PrintStream(out)));
      return new WithMockCompilerContext(out, compilerCfg);
    }
  }
}
