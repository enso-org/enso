package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class HashCodeTest extends TestBase {
  private static Context context;
  private static final InteropLibrary interop = InteropLibrary.getUncached();

  private static HashCodeNode hashCodeNode;
  private static EqualsNode equalsNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;
  private static TestRootNode testRootNode;

  @BeforeClass
  public static void initContextAndData() {
    context = createDefaultContext();
    executeInContext(
        context,
        () -> {
          hashCodeNode = HashCodeNode.build();
          equalsNode = EqualsNode.build();
          hostValueToEnsoNode = HostValueToEnsoNode.build();
          testRootNode = new TestRootNode();
          testRootNode.insertChildren(hashCodeNode, equalsNode, hostValueToEnsoNode);
          return null;
        });
    // Initialize datapoints here, to make sure that it is initialized just once.
    unwrappedValues = fetchAllUnwrappedValues();
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
  }

  /**
   * All values are static field, instead of method. Methods annotated with {@code DataPoints} may
   * be called multiple times, therefore, we should avoid this annotation for expensive methods.
   */
  @DataPoints public static Object[] unwrappedValues;

  private static Object[] fetchAllUnwrappedValues() {
    var valGenerator =
        ValuesGenerator.create(
            context, ValuesGenerator.Language.ENSO, ValuesGenerator.Language.JAVA);
    List<Value> values = new ArrayList<>();
    values.addAll(valGenerator.numbers());
    values.addAll(valGenerator.booleans());
    values.addAll(valGenerator.textual());
    values.addAll(valGenerator.arrayLike());
    values.addAll(valGenerator.vectors());
    values.addAll(valGenerator.maps());
    values.addAll(valGenerator.multiLevelAtoms());
    values.addAll(valGenerator.timesAndDates());
    values.addAll(valGenerator.timeZones());
    values.addAll(valGenerator.durations());
    values.addAll(valGenerator.periods());
    values.addAll(valGenerator.warnings());
    try {
      return values.stream()
          .map(value -> unwrapValue(context, value))
          .map(unwrappedValue -> hostValueToEnsoNode.execute(unwrappedValue))
          .collect(Collectors.toList())
          .toArray(new Object[] {});
    } catch (Exception e) {
      throw new AssertionError(e);
    }
  }

  @Theory
  public void hashCodeContractTheory(Object firstValue, Object secondValue) {
    executeInContext(
        context,
        () -> {
          long firstHash = hashCodeNode.execute(firstValue);
          long secondHash = hashCodeNode.execute(secondValue);
          Object valuesAreEqual = equalsNode.execute(firstValue, secondValue);
          // if o1 == o2 then hash(o1) == hash(o2)
          if (isTrue(valuesAreEqual)) {
            assertEquals(
                String.format(
                    """
                  If two objects are same, they should have same hash codes:
                    firstVal = %s, secondVal = %s, firstHash = %d, secondHash = %d
                  """,
                    interop.toDisplayString(firstValue),
                    interop.toDisplayString(secondValue),
                    firstHash,
                    secondHash),
                firstHash,
                secondHash);
          }
          // if hash(o1) != hash(o2) then o1 != o2
          if (firstHash != secondHash) {
            // Here, valuesAreEqual can either be False or Nothing
            assertTrue(
                "Violated rule: `if hash(o1) != hash(o2) then o1 != o2`",
                isFalse(valuesAreEqual) || isNothing(valuesAreEqual));
          }
          return null;
        });
  }

  @Theory
  public void hashCodeIsConsistent(Object value) {
    executeInContext(
        context,
        () -> {
          long firstHash = hashCodeNode.execute(value);
          long secondHash = hashCodeNode.execute(value);
          assertEquals("Hash code of an object should be consistent", firstHash, secondHash);
          return null;
        });
  }

  @Theory
  public void hashCodeCachedNodeIsConsistentWithUncached(Object value) {
    executeInContext(
        context,
        () -> {
          long uncachedRes = HashCodeNodeGen.getUncached().execute(value);
          long cachedRes = hashCodeNode.execute(value);
          assertEquals(
              "Result from cached HashCodeNode should be the same as from its uncached variant",
              uncachedRes,
              cachedRes);
          return null;
        });
  }

  private static boolean isTrue(Object obj) {
    return obj instanceof Boolean objBool && objBool;
  }

  private static boolean isFalse(Object obj) {
    return obj instanceof Boolean objBool && !objBool;
  }

  private static boolean isNothing(Object obj) {
    return obj == EnsoContext.get(null).getNothing();
  }
}
