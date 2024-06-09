package org.enso.interpreter.test;

import org.enso.interpreter.test.ValuesGenerator.Language;
import org.graalvm.polyglot.Context;

public final class MetaObjectPolyglotTest extends MetaObjectTest {
  public MetaObjectPolyglotTest() {}

  @Override
  protected ValuesGenerator createGenerator(Context ctx) {
    return ValuesGenerator.create(ctx, Language.values());
  }
}
