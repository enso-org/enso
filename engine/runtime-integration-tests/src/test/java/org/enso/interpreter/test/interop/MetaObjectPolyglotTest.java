package org.enso.interpreter.test.interop;

import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;

public final class MetaObjectPolyglotTest extends MetaObjectTest {
  public MetaObjectPolyglotTest() {}

  @Override
  protected ValuesGenerator createGenerator(ContextUtils ctx) {
    return ValuesGenerator.create(ctx, Language.values());
  }
}
