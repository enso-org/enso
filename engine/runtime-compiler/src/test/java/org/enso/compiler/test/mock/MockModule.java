package org.enso.compiler.test.mock;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;
import org.enso.common.CompilationStage;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.CompilerContext.ModuleScopeBuilder;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.IdMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;

final class MockModule extends CompilerContext.Module {

  private final QualifiedName qName;
  private final String code;
  private final String path;
  private final org.enso.pkg.Package<? extends Object> pkg;
  private final MockScopeBuilder scopeBuilder = new MockScopeBuilder();
  private final MockPackageRepository repo;
  private final boolean isSynthetic;
  private final List<QualifiedName> submoduleNames = new ArrayList<>();

  org.enso.compiler.core.ir.Module ir;
  BindingsMap bm;
  CompilationStage stage;

  /**
   * @param pkg Package that the module belongs to.
   * @param qName Qualified name of the module.
   * @param path Path to the file in the virtual FS.
   * @param code Content of the module
   * @param repo
   * @param isSynthetic
   */
  MockModule(
      Package<?> pkg,
      QualifiedName qName,
      String path,
      String code,
      MockPackageRepository repo,
      boolean isSynthetic) {
    this.pkg = pkg;
    this.qName = qName;
    this.path = path;
    this.code = code;
    this.stage = CompilationStage.INITIAL;
    this.repo = repo;
    this.isSynthetic = isSynthetic;
  }

  @Override
  public CharSequence getCharacters() throws IOException {
    return code;
  }

  /**
   * A dummy implementation. Computing line number for a file inside virtual file system that will
   * be deleted soon is not worth it.
   */
  @Override
  public int findLine(IdentifiedLocation loc) {
    return -1;
  }

  @Override
  public String getPath() {
    return path;
  }

  @Override
  public URI getUri() {
    return repo.getVfs().getUri(path);
  }

  @Override
  public Package<? extends Object> getPackage() {
    return pkg;
  }

  @Override
  public QualifiedName getName() {
    return qName;
  }

  @Override
  public BindingsMap getBindingsMap() {
    if (this.getIr() != null) {
      // move to better location than the context
      var meta = this.getIr().passData();
      var pass = meta.get(BindingAnalysis$.MODULE$);
      if (pass.isDefined()) {
        return (BindingsMap) pass.get();
      }
    }
    return bm;
  }

  @Override
  public IdMap getIdMap() {
    throw new UnsupportedOperationException();
  }

  @Override
  public List<QualifiedName> getDirectModulesRefs() {
    return submoduleNames;
  }

  public void addSubmodule(QualifiedName submoduleName) {
    submoduleNames.add(submoduleName);
  }

  @Override
  public CompilationStage getCompilationStage() {
    return stage;
  }

  @Override
  public boolean isSynthetic() {
    return isSynthetic;
  }

  @Override
  public Module getIr() {
    return ir;
  }

  @Override
  public boolean isPrivate() {
    return false;
  }

  @Override
  public CompilerContext.ModuleScopeBuilder getScopeBuilder() {
    return scopeBuilder;
  }

  @Override
  public CompilerContext.ModuleScopeBuilder newScopeBuilder() {
    return new MockScopeBuilder();
  }

  String getSourceSection(IdentifiedLocation location) {
    if (location != null) {
      return code.substring(location.start(), location.end());
    }
    return null;
  }

  @Override
  public String toString() {
    return "MockModule{qName='" + qName + "'}";
  }

  static final class MockScopeBuilder extends ModuleScopeBuilder {}
}
