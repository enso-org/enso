package org.enso.compiler.test.mock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.List;
import org.enso.pkg.QualifiedName;
import org.junit.Before;
import org.junit.Test;

public final class TestMockPackageRepository {

  private MockPackageRepository repo;

  @Before
  public void before() {
    repo = MockPackageRepository.create();
  }

  @Test
  public void canCreateModule() {
    var mod = repo.createModule(QualifiedName.fromString("local.Proj.Main"), "# Empty");
    assertThat(mod, is(notNullValue()));
    assertThat(mod.isSynthetic(), is(false));
    assertThat(mod.getName().toString(), is("local.Proj.Main"));
  }

  @Test
  public void submodulesCreateSyntheticModules() {
    var modName = "local.Project.A.B";
    repo.createModule(QualifiedName.fromString(modName), "# Empty");
    var parentMod = repo.getLoadedModule("local.Project.A");
    assertThat(parentMod.isDefined(), is(true));
    assertThat(parentMod.get().isSynthetic(), is(true));
  }

  @Test
  public void assignsSubmodulesAsDirectModuleRefs() {
    var modName = "local.Project.A.B";
    repo.createModule(QualifiedName.fromString(modName), "# Empty");
    var parentMod = repo.getLoadedModule("local.Project.A").get();
    var directModuleRefs = parentMod.getDirectModulesRefs();
    assertThat(
        "Has child module as direct module reference",
        directModuleRefs,
        is(List.of(QualifiedName.fromString(modName))));
  }
}
