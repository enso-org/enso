package org.enso.table.expressions;

import static org.mockito.Mockito.*;

import java.util.function.Function;

import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import static org.junit.Assert.assertEquals;

public class ExpressionVisitorImplTest {
    private Function<String, Value> getColumn;
    private Function<Object, Value> makeConstantColumn;

    @SuppressWarnings("unchecked")
    @Before
    public void setUp() {
        getColumn = (Function<String, Value>) mock(Function.class);
        makeConstantColumn = (Function<Object, Value>) mock(Function.class);
    }

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluate(
            expr, getColumn, makeConstantColumn, "test_module", "test_type", new String[]{});
    }

    @Test
    public void testAddition() {
        Value result = evaluate("1 + 2");
        assertEquals(3, result.asInt()); // Fix: Use .asInt() for correct comparison
    }
}
