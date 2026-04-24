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
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.ClassRule;
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
  @ClassRule public static final ContextUtils ctxRule = ContextUtils.createDefault();

  private final TypeWithWrapper typeWithWrapper;

  public TypesExposeConstructorsTest(TypeWithWrapper typeWithWrapper) {
    this.typeWithWrapper = typeWithWrapper;
  }

  @Parameters(name = "{index}: {0}")
  public static Iterable<TypeWithWrapper> collectTypes() {
    var collectedTypes = new ArrayList<TypeWithWrapper>();
    try (ValuesGenerator valuesGenerator = ValuesGenerator.create(ctxRule, Language.ENSO)) {
      valuesGenerator.allTypes().stream()
          .map(
              tp -> {
                var unwrappedTp = ctxRule.unwrapValue(tp);
                if (unwrappedTp instanceof Type type) {
                  return new TypeWithWrapper(type, tp);
                } else {
                  return null;
                }
              })
          .filter(Objects::nonNull)
          .filter(tp -> !tp.type.getConstructors().isEmpty())
          .forEach(collectedTypes::add);
    } catch (Exception e) {
      throw new AssertionError(e);
    }
    return collectedTypes;
  }

  @After
  public void closeGenerator() throws Exception {
    typeWithWrapper.close();
  }

  @Test
  public void typesExposeConstructorsAsMembers() {
    var type = typeWithWrapper.type;
    var typeValue = typeWithWrapper.typeValue;
    var consNames = type.getConstructors().keySet();
    var isPrivate = type.hasAllConstructorsPrivate();
    for (var consName : consNames) {
      assertThat(
          "Constructor " + consName + " should be exposed as a member",
          typeValue.hasMember(consName),
          is(true));
      var consMember = typeValue.getMember(consName);
      assertThat(consMember, is(notNullValue()));
      if (!isPrivate) {
        assertThat(
            "Public constructor " + consName + " should be instantiable",
            consMember.canInstantiate(),
            is(true));
      }
    }
  }

  private static final class TypeWithWrapper implements AutoCloseable {
    private Type type;
    private Value typeValue;

    /**
     * @param tp The polyglot value of the type (not an object)
     */
    public TypeWithWrapper(Type type, Value tp) {
      this.type = type;
      this.typeValue = tp;
    }

    @Override
    public void close() {
      type = null;
      typeValue = null;
    }

    @Override
    public String toString() {
      return "TypeWithWrapper(" + type.getQualifiedName() + "}";
    }
  }
}
