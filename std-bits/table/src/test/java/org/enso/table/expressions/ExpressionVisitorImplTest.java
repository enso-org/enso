import static org.mockito.Mockito.*;

import java.util.function.Function;

import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.quality.Strictness;

import org.enso.table.expressions.ExpressionVisitorImpl;

import static org.junit.Assert.assertEquals;

@ExtendWith(MockitoExtension.class)
public class ExpressionVisitorImplTest {

    @Rule
    public MockitoRule mockitoRule = MockitoJUnit.rule().strictness(Strictness.STRICT_STUBS);

    @Mock private Function<String, Value> getColumn;
    @Mock private Function<Object, Value> makeConstantColumn;
    @Mock private Function<String, Value> getMethod;
    @Mock private Function<String, Value> makeConstructor;

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluateImpl(
            expr, getColumn, makeConstantColumn, getMethod, makeConstructor, new String[]{});
    }

    @Test
    public void testIntegerConstant() {
        Value result = evaluate("1");
        assertEquals(1, result.asInt());
    }

    @Test
    public void testSimpleExpressionOnColumn() {
        Value mockedColumn = mock(Value.class);
        Value mockedMethod = mock(Value.class);
        Value mockedResult = mock(Value.class);

        when(getColumn.apply("Column 1")).thenReturn(mockedColumn);
        when(getMethod.apply("text_length")).thenReturn(mockedMethod);
        when(mockedMethod.canExecute()).thenReturn(true);
        when(mockedMethod.execute(mockedColumn)).thenReturn(mockedResult);
        when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedResult);

        Value result = evaluate("text_length([Column 1])");
        assertEquals(mockedResult, result);
    }
}
