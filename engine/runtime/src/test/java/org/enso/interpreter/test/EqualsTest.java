package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.math.BigInteger;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNodeGen;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class EqualsTest extends TestBase {
  private static Context context;
  private static EqualsNode equalsNode;
  private static TestRootNode testRootNode;
  private static HostValueToEnsoNode hostValueToEnsoNode;

  @BeforeClass
  public static void initContextAndData() {
    context = createDefaultContext();
    executeInContext(
        context,
        () -> {
          testRootNode = new TestRootNode();
          equalsNode = EqualsNode.build();
          hostValueToEnsoNode = HostValueToEnsoNode.build();
          testRootNode.insertChildren(equalsNode, hostValueToEnsoNode);
          return null;
        });
    unwrappedValues = fetchAllUnwrappedValues();
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
  }

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
  public void equalsOperatorShouldBeSymmetric(Object firstValue, Object secondValue) {
    executeInContext(
        context,
        () -> {
          boolean firstResult = equalsNode.execute(firstValue, secondValue);
          boolean secondResult = equalsNode.execute(secondValue, firstValue);
          assertEquals("equals should be symmetric", firstResult, secondResult);
          return null;
        });
  }

  @Theory
  public void equalsOperatorShouldBeConsistent(Object value) {
    executeInContext(
        context,
        () -> {
          Object firstResult = equalsNode.execute(value, value);
          Object secondResult = equalsNode.execute(value, value);
          assertEquals("equals should be consistent", firstResult, secondResult);
          return null;
        });
  }

  @Theory
  public void equalsNodeCachedIsConsistentWithUncached(Object firstVal, Object secondVal) {
    executeInContext(
        context,
        () -> {
          Object uncachedRes = EqualsNodeGen.getUncached().execute(firstVal, secondVal);
          Object cachedRes = equalsNode.execute(firstVal, secondVal);
          if (uncachedRes != cachedRes) {
            Thread.dumpStack();
          }
          assertEquals(
              "Result from uncached EqualsNode should be the same as result from its cached variant",
              uncachedRes,
              cachedRes);
          return null;
        });
  }

  /** Test for some specific values, for which we know that they are equal. */
  @Test
  public void testDateEquality() {
    Object ensoDate =
        unwrapValue(
            context,
            createValue(
                context, "(Date.new 1999 3 23)", "import Standard.Base.Data.Time.Date.Date"));
    Object javaDate = unwrapValue(context, context.asValue(LocalDate.of(1999, 3, 23)));
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoDate, javaDate));
          return null;
        });
  }

  @Test
  public void testTimeEquality() {
    Object ensoTime =
        unwrapValue(
            context,
            createValue(
                context,
                "Time_Of_Day.new 23 59",
                "import Standard.Base.Data.Time.Time_Of_Day.Time_Of_Day"));
    Object javaDate = unwrapValue(context, context.asValue(LocalTime.of(23, 59)));
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoTime, javaDate));
          return null;
        });
  }

  @Test
  public void testDateTimeEquality() {
    Object ensoDateTime =
        unwrapValue(
            context,
            createValue(
                context,
                "(Date_Time.new 1999 3 1 23 59)",
                "import Standard.Base.Data.Time.Date_Time.Date_Time"));
    Object javaDateTime =
        unwrapValue(
            context,
            context.asValue(
                ZonedDateTime.of(
                    LocalDate.of(1999, 3, 1), LocalTime.of(23, 59), ZoneId.systemDefault())));
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoDateTime, javaDateTime));
          return null;
        });
  }

  @Test
  public void testVectorsEquality() {
    Object ensoVector =
        unwrapValue(context, createValue(context, "[1,2,3]", "from Standard.Base import all"));
    Object javaVector = unwrapValue(context, context.asValue(List.of(1L, 2L, 3L)));
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoVector, javaVector));
          return null;
        });
  }

  @ExportLibrary(InteropLibrary.class)
  static final class WrappedPrimitive implements TruffleObject {
    private final Object value;

    WrappedPrimitive(long value) {
      this.value = value;
    }

    WrappedPrimitive(boolean value) {
      this.value = value;
    }

    WrappedPrimitive(double value) {
      this.value = value;
    }

    WrappedPrimitive(BigInteger value) {
      this.value = value;
    }

    @ExportMessage
    boolean isNumber() {
      return value instanceof Number;
    }

    @ExportMessage
    boolean isBoolean() {
      return value instanceof Boolean;
    }

    @ExportMessage
    boolean asBoolean() {
      return (Boolean) value;
    }

    @ExportMessage
    boolean fitsInByte() {
      return false;
    }

    @ExportMessage
    boolean fitsInShort() {
      return false;
    }

    @ExportMessage
    boolean fitsInInt() {
      return false;
    }

    @ExportMessage
    boolean fitsInLong() {
      return value instanceof Long;
    }

    @ExportMessage
    boolean fitsInFloat() {
      return false;
    }

    @ExportMessage
    boolean fitsInDouble() {
      return value instanceof Double;
    }

    @ExportMessage
    byte asByte() throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    short asShort() throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    int asInt() throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    long asLong() throws UnsupportedMessageException {
      return (Long) value;
    }

    @ExportMessage
    float asFloat() throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    double asDouble() throws UnsupportedMessageException {
      return (Double) value;
    }

    @ExportMessage
    boolean fitsInBigInteger() {
      return value instanceof BigInteger;
    }

    @ExportMessage
    BigInteger asBigInteger() throws UnsupportedMessageException {
      return (BigInteger) value;
    }

    Object asDirect() {
      return value;
    }
  }

  @Test
  public void testTruffleNumberLong() {
    var ensoNumber = unwrapValue(context, createValue(context, "1", ""));
    var foreignNumber = new WrappedPrimitive(1);
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoNumber, foreignNumber.asDirect()));
          assertTrue(equalsNode.execute(ensoNumber, foreignNumber));
          assertTrue(equalsNode.execute(foreignNumber, ensoNumber));
          return null;
        });
  }

  @Test
  public void testTruffleNumberDouble() {
    var ensoNumber = unwrapValue(context, createValue(context, "1.0", ""));
    var foreignNumber = new WrappedPrimitive(1.0);
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoNumber, foreignNumber.asDirect()));
          assertTrue(equalsNode.execute(ensoNumber, foreignNumber));
          assertTrue(equalsNode.execute(foreignNumber, ensoNumber));
          return null;
        });
  }

  @Test
  public void testTruffleNumberBigInt() {
    var value = new BigInteger("43207431473298432194374819743291479009431478329");
    var ensoNumber = unwrapValue(context, createValue(context, value.toString(), ""));
    var foreignNumber = new WrappedPrimitive(value);
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoNumber, foreignNumber));
          assertTrue(equalsNode.execute(foreignNumber, ensoNumber));
          return null;
        });
  }

  @Test
  public void testTruffleBoolean() {
    var ensoBoolean =
        unwrapValue(context, createValue(context, "True", "from Standard.Base import True"));
    var foreignBoolean = new WrappedPrimitive(true);
    executeInContext(
        context,
        () -> {
          assertTrue(equalsNode.execute(ensoBoolean, foreignBoolean.asDirect()));
          assertTrue(equalsNode.execute(ensoBoolean, foreignBoolean));
          assertTrue(equalsNode.execute(foreignBoolean, ensoBoolean));
          return null;
        });
  }
}
