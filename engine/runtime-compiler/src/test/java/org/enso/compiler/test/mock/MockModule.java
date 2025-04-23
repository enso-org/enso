package org.enso.compiler.test.mock;

import java.io.IOException;
import java.net.URI;
import org.enso.common.CompilationStage;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.IdMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;

public final class MockModule extends CompilerContext.Module {

  private final QualifiedName qName;
  private final String code;
  private final String path;
  private final org.enso.pkg.Package<? extends Object> pkg;

  org.enso.compiler.core.ir.Module ir;
  BindingsMap bm;
  CompilationStage stage;

  public MockModule(org.enso.pkg.Package<?> pkg, QualifiedName qName, String path, String code) {
    this.pkg = pkg;
    this.qName = qName;
    this.path = path;
    this.code = code;
    this.stage = CompilationStage.INITIAL;
  }

  @Override
  public CharSequence getCharacters() throws IOException {
    return code;
  }

  @Override
  public int findLine(IdentifiedLocation loc) {
    throw new UnsupportedOperationException();
  }

  @Override
  public String getPath() {
    return path;
  }

  @Override
  public URI getUri() {
    throw new UnsupportedOperationException();
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
  public java.util.List<QualifiedName> getDirectModulesRefs() {
    return java.util.List.of();
  }

  @Override
  public CompilationStage getCompilationStage() {
    return stage;
  }

  @Override
  public boolean isSynthetic() {
    return false;
  }

  @Override
  public Module getIr() {
    return ir;
  }

  @Override
  public boolean isPrivate() {
    throw new UnsupportedOperationException();
  }

  @Override
  public CompilerContext.ModuleScopeBuilder getScopeBuilder() {
    throw new UnsupportedOperationException();
  }

  @Override
  public CompilerContext.ModuleScopeBuilder newScopeBuilder() {
    throw new UnsupportedOperationException();
  }
}
