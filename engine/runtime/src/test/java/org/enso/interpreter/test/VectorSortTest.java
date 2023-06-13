package org.enso.interpreter.test;

import java.util.ArrayList;
import java.util.List;

import org.enso.interpreter.test.ValuesGenerator.Language;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Assume;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

/**
 * This test ensures that any combination of values can be sorted - no attempt to sort should fail.
 */
@RunWith(Theories.class)
public class VectorSortTest extends TestBase {
  private static Context context;
  private static Value sortFunc;
  private static Value equalsFunc;

  @BeforeClass
  public static void initCtxAndNodes() {
    context = createDefaultContext();
    var code =
        """
    from Standard.Base import all

    sort val1 val2 = [val1, val2].sort
    equals val1 val2 = val1 == val2
    """;
    sortFunc = getMethodFromModule(context, code, "sort");
    equalsFunc = getMethodFromModule(context, code, "equals");

    values = new ArrayList<>();
    var valuesGenerator =
        ValuesGenerator.create(context, Language.ENSO, Language.JAVA);
    values.addAll(valuesGenerator.numbers());
    values.addAll(valuesGenerator.vectors());
    values.addAll(valuesGenerator.arrayLike());
    values.addAll(valuesGenerator.booleans());
    values.addAll(valuesGenerator.durations());
    values.addAll(valuesGenerator.maps());
  }

  @AfterClass
  public static void disposeCtx() {
    context.close();
  }

  @DataPoints public static List<Value> values;

  @Theory
  public void testSortHandlesAllValues(Value value1, Value value2) {
    Assume.assumeFalse(isNan(value1) || isNan(value2));
    Value res = sortFunc.execute(value1, value2);
    assertTrue(res.hasArrayElements());
    assertEquals(2, res.getArraySize());
    List<Value> resArray = readPolyglotArray(res);
    // check that value1 is there unchanged on some index, and the same for value2
    assertTrue(
        "Sorted vector should contain the first value at any index",
        invokeEquals(value1, resArray.get(0)) || invokeEquals(value1, resArray.get(1)));
    assertTrue(
        "Sorted vector should contain the second value at any index",
        invokeEquals(value2, resArray.get(0)) || invokeEquals(value2, resArray.get(1)));
  }

  private boolean isNan(Value value) {
    if (value.isNumber() && value.fitsInDouble()) {
      return Double.isNaN(value.asDouble());
    } else {
      return false;
    }
  }

  private List<Value> readPolyglotArray(Value array) {
    assertTrue(array.hasArrayElements());
    assertTrue(array.hasIterator());
    Value iterator = array.getIterator();
    assertTrue(iterator.isIterator());
    List<Value> res = new ArrayList<>();
    while (iterator.hasIteratorNextElement()) {
      res.add(iterator.getIteratorNextElement());
    }
    return res;
  }

  private boolean invokeEquals(Value val1, Value val2) {
    Value res = equalsFunc.execute(val1, val2);
    assertTrue("Result from Any.== should be boolean", res.isBoolean());
    return res.asBoolean();
  }
}
