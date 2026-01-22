package org.enso.example.deps;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ForkJoinPool;
import org.enso.example.deps.one.DepAClass;
import org.enso.example.deps.two.DepBClass;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class ClassLoadingWorksTest {
    @Test
    public void depADepB() throws Exception {
    List<Callable<Double>> cases = new ArrayList<>();
    cases.add(() -> new DepAClass().expToNeg(2));
    cases.add(() -> new DepBClass().sigmoid(2));
    var results = ForkJoinPool.commonPool().invokeAll(cases);
    assertEquals(results.get(0).get().doubleValue(), 0.1353352832366127, 0);
    assertEquals(results.get(1).get().doubleValue(), 0.8807970779778823, 0);
    }
}
