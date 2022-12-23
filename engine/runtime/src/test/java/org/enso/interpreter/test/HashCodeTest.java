package org.enso.interpreter.test;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.nio.file.Paths;
import java.util.List;
import java.util.Map;
import org.enso.polyglot.MethodNames.Module;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class HashCodeTest {
  private Context ctx;
  private Value equalsFunc;
  private Value hashCodeFunc;
  private ValuesGenerator valGenerator;

  @Before
  public void initializeContext() {
    this.ctx = Context.newBuilder("enso")
        .allowExperimentalOptions(true)
        .allowIO(true)
        .allowAllAccess(true)
        .logHandler(new ByteArrayOutputStream())
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath()
        ).build();
    final Map<String, Language> langs = ctx.getEngine().getLanguages();
    assertNotNull("Enso found: " + langs, langs.get("enso"));
    var src = Source.newBuilder("enso", """
        equals x y = x == y
        hashCode x = x.hash_code
        """, "tmp.enso").buildLiteral();
    var module = ctx.eval(src);
    equalsFunc = module.invokeMember(Module.EVAL_EXPRESSION, "equals");
    hashCodeFunc = module.invokeMember(Module.EVAL_EXPRESSION, "hashCode");
    assertTrue(equalsFunc.canExecute());
    assertTrue(hashCodeFunc.canExecute());
    this.valGenerator = ValuesGenerator.create(
        ctx,
        ValuesGenerator.Language.ENSO,
        ValuesGenerator.Language.JAVA,
        ValuesGenerator.Language.JAVASCRIPT,
        ValuesGenerator.Language.PYTHON
    );
  }

  @After
  public void closeContext() {
    this.ctx.close();
  }

  @Test
  public void testPrimitives() {
    checkHashContract(valGenerator.booleans());
    checkHashContract(valGenerator.numbers());
    checkHashContract(valGenerator.textual());
  }
  @Test
  public void testVectors() {
    checkHashContract(valGenerator.vectors());
  }

  private void checkHashContract(List<Value> values) {
    checkHashContract(values, values);
  }

  /**
   * All the elements of firstVals and secondVals should have same type.
   * <p>
   * See the description of hashing contract
   * in {@link org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode}.
   */
  private void checkHashContract(List<Value> firstVals, List<Value> secondVals) {
    for (Value firstVal : firstVals) {
      for (Value secondVal : secondVals) {
        long firstHash = valueHash(firstVal);
        long secondHash = valueHash(secondVal);
        if (firstHash == secondHash) {
          assertTrue(
              String.format("""
              If hash codes of two objects are same, they should be equal:
                firstVal = %s, secondVal = %s, firstHash = %d, secondHash = %d
              """, firstVal, secondVal, firstHash, secondHash),
              valuesEqual(firstVal, secondVal)
          );
        } else {
          assertFalse(
              String.format("""
              If hash codes of two objects are different, they should not be equal:
                firstVal = %s, secondVal = %s, firstHash = %d, secondHash = %d
              """, firstVal, secondVal, firstHash, secondHash),
              valuesEqual(firstVal, secondVal)
          );
        }
      }
    }
  }

  private long valueHash(Value value) {
    Value hash = hashCodeFunc.execute(value);
    assertTrue(
        "Any.hash_code should return long, but returned " + hash,
        hash.fitsInLong()
    );
    return hash.asLong();
  }

  private boolean valuesEqual(Value val1, Value val2) {
    Value equalsRes = equalsFunc.execute(val1, val2);
    assertTrue(
        "Any.== should return boolean, but returned " + equalsRes,
        equalsRes.isBoolean()
    );
    return equalsRes.asBoolean();
  }
}
