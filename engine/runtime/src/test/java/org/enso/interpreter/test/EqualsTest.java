package org.enso.interpreter.test;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.runner.RunWith;

@RunWith(Theories.class)
public class EqualsTest extends TestBase {
  private static Context context;
  private EqualsAnyNode equalsNode;

  @BeforeClass
  public static void initContextAndData() {
    context = createDefaultContext();
    unwrappedValues = fetchAllUnwrappedValues();
  }

  @Before
  public void initNodes() {
    executeInContext(
        context,
        () -> {
          equalsNode = EqualsAnyNode.build();
          return null;
        });
  }

  @AfterClass
  public static void disposeContext() {
    context.close();
  }

  @DataPoints public static Object[] unwrappedValues;

  private static Object[] fetchAllUnwrappedValues() {
    var valGenerator =
        ValuesGenerator.create(
            context,
            ValuesGenerator.Language.ENSO,
            ValuesGenerator.Language.JAVA,
            ValuesGenerator.Language.JAVASCRIPT,
            ValuesGenerator.Language.PYTHON);
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
          boolean secondResult = equalsNode.execute(firstValue, secondValue);
          assertEquals("equals should be symmetric", firstResult, secondResult);
          return null;
        });
  }

  @Theory
  public void equalsOperatorShouldBeConsistent(Object value) {
    executeInContext(
        context,
        () -> {
          boolean firstResult = equalsNode.execute(value, value);
          boolean secondResult = equalsNode.execute(value, value);
          assertEquals("equals should be consistent", firstResult, secondResult);
          return null;
        });
  }
}
