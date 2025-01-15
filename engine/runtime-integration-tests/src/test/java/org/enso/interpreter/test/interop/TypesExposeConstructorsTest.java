package org.enso.interpreter.test.interop;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.ArrayList;
import java.util.Objects;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.test.ValuesGenerator;
import org.enso.interpreter.test.ValuesGenerator.Language;
import org.enso.test.utils.ContextUtils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

/**
 * Tests that every {@link org.enso.interpreter.runtime.data.Type} exposes its constructors as
 * {@link com.oracle.truffle.api.interop.InteropLibrary#getMembers(Object) members}, and that such
 * members are {@link com.oracle.truffle.api.interop.InteropLibrary#isInstantiable(Object)
 * instantiable}.
 */
@RunWith(Parameterized.class)
public class TypesExposeConstructorsTest {
  private static Context ctx;

  private final TypeWithWrapper typeWithWrapper;

  public TypesExposeConstructorsTest(TypeWithWrapper typeWithWrapper) {
    this.typeWithWrapper = typeWithWrapper;
  }

  private static Context ctx() {
    if (ctx == null) {
      ctx = ContextUtils.createDefaultContext();
    }
    return ctx;
  }

  @AfterClass
  public static void disposeCtx() {
    if (ctx != null) {
      ctx.close();
      ctx = null;
    }
  }

  @Parameters(name = "{index}: {0}")
  public static Iterable<TypeWithWrapper> collectTypes() {
    var collectedTypes = new ArrayList<TypeWithWrapper>();
    ContextUtils.executeInContext(
        ctx(),
        () -> {
          var valuesGenerator = ValuesGenerator.create(ctx(), Language.ENSO);
          valuesGenerator.allTypes().stream()
              .map(
                  tp -> {
                    var unwrappedTp = ContextUtils.unwrapValue(ctx(), tp);
                    if (unwrappedTp instanceof Type type) {
                      return new TypeWithWrapper(type, tp);
                    } else {
                      return null;
                    }
                  })
              .filter(Objects::nonNull)
              .filter(tp -> !tp.type.getConstructors().isEmpty())
              .forEach(collectedTypes::add);
          return null;
        });
    return collectedTypes;
  }

  @Test
  public void typesExposeConstructorsAsMembers() {
    var type = typeWithWrapper.type;
    var typeValue = typeWithWrapper.typeValue;
    var consNames = type.getConstructors().keySet();
    for (var consName : consNames) {
      assertThat(
          "Constructor " + consName + " should be exposed as a member",
          typeValue.hasMember(consName),
          is(true));
      var consMember = typeValue.getMember(consName);
      assertThat(consMember, is(notNullValue()));
      assertThat(
          "Constructor " + consName + " should be instantiable",
          consMember.canInstantiate(),
          is(true));
    }
  }

  /**
   * @param type
   * @param typeValue The polyglot value of the type (not an object)
   */
  public record TypeWithWrapper(Type type, Value typeValue) {}
}
