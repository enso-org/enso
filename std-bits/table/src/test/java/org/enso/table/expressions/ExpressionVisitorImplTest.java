package org.enso.table.expressions;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.function.Function;
import org.enso.table.expressions.ExpressionVisitorImpl.Method;
import org.graalvm.polyglot.Value;
import org.junit.Rule;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
public class ExpressionVisitorImplTest {

  @Rule public MockitoRule mockitoRule = MockitoJUnit.rule().strictness(Strictness.STRICT_STUBS);

  @Mock private Function<String, Value> getColumn;
  @Mock private Function<Object, Value> makeConstantColumn;
  @Mock private Function<String, Method> getMethod;
  @Mock private Function<String, Value> makeConstructor;

  private Value evaluate(String expr) {
    return ExpressionVisitorImpl.evaluateImpl(
        expr, getColumn, makeConstantColumn, getMethod, makeConstructor);
  }

  @Test
  public void testIntegerConstant() {
    Value result = evaluate("1");
    assertEquals(1, result.asInt());
  }

  @Test
  public void testSimpleMethodOnColumn() {
    Value mockedColumn1 = mock(Value.class);
    Method mockedMethodTextLength = mock(Method.class);
    Value mockedEnsoMethodTextLength = mock(Value.class);
    Value mockedResult = mock(Value.class);

    when(getColumn.apply("Column 1")).thenReturn(mockedColumn1);
    when(getMethod.apply("text_length")).thenReturn(mockedMethodTextLength);
    when(mockedMethodTextLength.isVariableArgumentMethod()).thenReturn(false);
    when(mockedMethodTextLength.isStaticMethod()).thenReturn(false);
    when(mockedMethodTextLength.getEnsoMethod()).thenReturn(mockedEnsoMethodTextLength);
    when(mockedEnsoMethodTextLength.execute(mockedColumn1)).thenReturn(mockedResult);
    when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedResult);

    Value result = evaluate("text_length([Column 1])");
    assertEquals(mockedResult, result);
  }

  @Test
  public void testSimpleStaticMethod() {
    Method mockedMethodToday = mock(Method.class);
    Value mockedEnsoMethodToday = mock(Value.class);
    Value mockedResult = mock(Value.class);
    Value mockedStaticsType = mock(Value.class);

    when(getMethod.apply("today")).thenReturn(mockedMethodToday);
    when(mockedMethodToday.getEnsoMethod()).thenReturn(mockedEnsoMethodToday);
    when(mockedMethodToday.isVariableArgumentMethod()).thenReturn(false);
    when(mockedMethodToday.isStaticMethod()).thenReturn(true);
    when(mockedMethodToday.getStaticsType()).thenReturn(mockedStaticsType);
    // Static method require passing their type as the synthetic 'self' argument
    when(mockedEnsoMethodToday.execute(mockedStaticsType)).thenReturn(mockedResult);
    when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedResult);

    Value result = evaluate("today()");
    assertEquals(mockedResult, result);
  }
}
