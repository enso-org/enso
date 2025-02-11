package org.enso.table.expressions;

import static org.mockito.Mockito.*;

import java.util.function.Function;

import org.graalvm.polyglot.Value;
import org.junit.Test;
import org.junit.jupiter.api.BeforeEach;

import org.mockito.Mockito;

import static org.junit.Assert.assertEquals;

public class ExpressionVisitorImplTest {
private Function<String, Value> getColumn;
    private Function<Object, Value> makeConstantColumn;

    @BeforeEach
    void setUp() {
        getColumn = Mockito.mock(Function.class);
        makeConstantColumn = Mockito.mock(Function.class);
    }

    private Value evaluate(String expr) {
        return ExpressionVisitorImpl.evaluate(
            expr, getColumn, makeConstantColumn, "test_module", "test_type", new String[]{});
    }

    @Test
    public void testAddition() {
        Value result = evaluate("1 + 2");
        assertEquals(4, result);
    }
}