package org.enso.interpreter.test.closure;

import java.util.Collection;
import java.util.List;
import org.junit.Ignore;

@Ignore
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
    return 25;
  }

  @Override
  protected int maximumOfModules() {
    return 35;
  }

  @Override
  protected Collection<String> disallowModules() {
    return List.of("Standard.Base.Data.Text.Text_Cleanse", "Standard.Base.Meta.Enso_Project");
  }
}
