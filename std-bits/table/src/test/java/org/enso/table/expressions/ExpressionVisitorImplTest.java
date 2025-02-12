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
        getColumn = (Function<String, Value>) mock(Function.class);
        makeConstantColumn = (Function<Object, Value>) mock(Function.class);
        getMethod = (Function<String, Value>) mock(Function.class);
        makeConstructor = (Function<String, Value>) mock(Function.class);
    }

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluateImpl(
            expr, getColumn, makeConstantColumn, getMethod, makeConstructor, new String[]{});
    }

    @Test
    public void testAddition() {
        Value result = evaluate("1");
        assertEquals(1, result.asInt());
    }
}
