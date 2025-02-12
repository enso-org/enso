import java.nio.file.Paths;

import static org.mockito.Mockito.*;

import java.util.function.Function;
import java.util.logging.Level;

import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import org.enso.table.expressions.ExpressionVisitorImpl;

import static org.junit.Assert.assertEquals;

public class ExpressionVisitorImplTest {
    private Function<String, Value> getColumn;
    private Function<Object, Value> makeConstantColumn;
    private Function<String, Value> getMethod;
    private Function<String, Value> makeConstructor;

    @SuppressWarnings("unchecked")
    @Before
    public void setUp() {
        Mockito.framework().clearInlineMocks();
        getColumn = (Function<String, Value>) mock(Function.class);
        makeConstantColumn = (Function<Object, Value>) mock(Function.class);
        getMethod = (Function<String, Value>) mock(Function.class);
        makeConstructor = (Function<String, Value>) mock(Function.class);
    }

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluateImpl(
            expr, getColumn, makeConstantColumn, getMethod, makeConstructor, new String[]{});
    }

    private Value mockValue(int value) {
        Value val = mock(Value.class);
        when(val.asInt()).thenReturn(value);
        when(val.isNumber()).thenReturn(true);
        return val;
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
        Value mockedResult = mockValue(5);

        when(getColumn.apply("Column 1")).thenReturn(mockedColumn);
        when(getMethod.apply("text_length")).thenReturn(mockedMethod);
        when(mockedMethod.canExecute()).thenReturn(true);
        when(mockedMethod.execute(mockedColumn)).thenReturn(mockedResult);
        when(makeConstantColumn.apply(mockedResult)).thenReturn(mockedResult);

        Value result = ExpressionVisitorImpl.evaluateImpl(
            "text_length([Column 1])", getColumn, makeConstantColumn, getMethod, makeConstructor, new String[]{});

        assertEquals(5, result.asInt());
    }
}
