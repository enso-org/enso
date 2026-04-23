package org.enso.interpreter.test.closure;

import java.util.Collection;
import java.util.List;

public final class AnyClosureTest extends TransitiveInfra {
  @Override
  protected String moduleName() {
    return "Standard.Base.Any.Any";
  }

  @Override
  protected String code() {
    var code =
        """
        import Standard.Base.Any.Any
        main = Any
        """;
    return code;
  }

  @Override
  protected int minimumOfModules() {
    return 3;
  }

  @Override
  protected int maximumOfModules() {
    return 6;
  }

  @Override
  protected Collection<String> disallowModules() {
    return List.of("Standard.Base.Data.Text.Text_Cleanse", "Standard.Base.Meta.Enso_Project");
  }
}
