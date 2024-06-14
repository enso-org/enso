package org.enso.compiler.pass.analyse.types.scope;

import java.util.List;

public final class AtomType {
  private final String name;
  private final StaticModuleScope definingScope;

  public AtomType(String name, List<Constructor> constructors, StaticModuleScope definingScope) {
    this.name = name;
    this.definingScope = definingScope;
  }

  StaticModuleScope getDefiningScope() {
    return definingScope;
  }

  public String getName() {
    return name;
  }

  // This is a sibling to BindingsMap.Cons. For now kept separate on purpose.
  // TODO do we want arguments or a signature here?
  public record Constructor(String name, boolean isProjectPrivate) {}
}
