package org.enso.compiler.test.mock;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Set;
import org.apache.commons.vfs2.FileObject;
import org.enso.compiler.Compiler;
import org.enso.compiler.context.CompilerContext.Module;
import org.enso.compiler.data.CompilerConfig;
import org.enso.editions.LibraryName;
import org.enso.pkg.Package;
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
  private final PrintStream out;
  private final CompilerConfig compilerCfg;
  private final MockCompilerContext compilerContext;
  private final Compiler compiler;

  private WithMockCompilerContext(PrintStream out, CompilerConfig compilerCfg) {
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

  public MockPackageRepository getRepo() {
    return repo;
  }

  public Compiler getCompiler() {
    return compiler;
  }

  public Package<FileObject> createPackage(LibraryName pkgName, SourceModule module) {
    return repo.createPackage(pkgName, module);
  }

  /**
   * Create a package with the given set of source modules in the virtual file system.
   *
   * @param pkgName
   * @param modules
   * @return Created package, or null if an error occurred.
   */
  public Package<FileObject> createPackage(LibraryName pkgName, Set<SourceModule> modules) {
    return repo.createPackage(pkgName, modules);
  }

  public void registerMainProjectPackage(LibraryName pkgName, Package<FileObject> mainPkg) {
    repo.registerMainProjectPackage(pkgName, MockPackageRepository.castVirtualPkg(mainPkg));
  }

  public List<Module> getLoadedModules() {
    return CollectionConverters.asJava(repo.getLoadedModules());
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
        var allFiles = repo.listAllFilesInVfs();
        System.err.println("=== All files in VFS === ");
        System.err.println(allFiles);
        System.err.println("=== End of VFS === ");
        throw e;
      } finally {
        repo.getVfs().deleteAll();
      }
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
      var out = new PrintStream(new ByteArrayOutputStream());
      var compilerCfg =
          new CompilerConfig(
              true,
              enableWarnings,
              true,
              enableStaticTypeInference,
              scala.Option.empty(),
              isStringErrors,
              enableLinting,
              scala.Some.apply(out));
      return new WithMockCompilerContext(out, compilerCfg);
    }
  }
}
