package org.enso.table.data.index;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.data.table.problems.FloatingPointGrouping;
import org.enso.table.error.TooManyColumnsException;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.util.ConstantList;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;

public class ObjectNumberer<T> {
    private final Map<T, Integer> numbering = new HashMap<>();
    private int serial = 0;

    public ObjectNumberer(Collection<T> ts) {
        putAll(ts);
    }

    public synchronized void put(T t) {
        if (!numbering.containsKey(t)) {
            //System.out.println("Putting " + t + " " + serial);
            numbering.put(t, serial);
            serial++;
        }
    }

    public void putAll(Collection<T> ts) {
        for (T t : ts) {
            put(t);
        }
    }

    public int getNumber(T t) {
        //System.out.println("Getting " + t);
        return numbering.get(t);
    }

    public int size() {
        return numbering.size();
    }

    public Set<T> getObjects() {
        return numbering.keySet();
    }
}
