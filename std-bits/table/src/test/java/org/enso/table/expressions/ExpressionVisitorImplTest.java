package org.enso.table.expressions;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.function.Function;
import org.enso.table.expressions.ExpressionVisitorImpl.MethodInterface;
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
  @Mock private Function<String, MethodInterface> getMethod;
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
    MethodInterface mockedMethodTextLength = mock(MethodInterface.class);
    Value mockedResult = mock(Value.class);
    Value mockedColumnResult = mock(Value.class);

    when(getColumn.apply("Column 1")).thenReturn(mockedColumn1);
    when(getMethod.apply("text_length")).thenReturn(mockedMethodTextLength);
    when(mockedMethodTextLength.execute(new Value[] {mockedColumn1}, makeConstantColumn))
        .thenReturn(mockedResult);
    when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedColumnResult);

    Value result = evaluate("text_length([Column 1])");
    assertEquals(mockedColumnResult, result);
  }

  @Test
  public void testSimpleStaticMethod() {
    MethodInterface mockedMethodToday = mock(MethodInterface.class);
    Value mockedResult = mock(Value.class);
    Value mockedColumnResult = mock(Value.class);

    when(getMethod.apply("today")).thenReturn(mockedMethodToday);
    when(mockedMethodToday.execute(new Value[] {}, makeConstantColumn)).thenReturn(mockedResult);
    when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedColumnResult);

    Value result = evaluate("today()");
    assertEquals(mockedColumnResult, result);
  }
}
