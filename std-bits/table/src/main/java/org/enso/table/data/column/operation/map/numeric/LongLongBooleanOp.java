package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.AbstractLongStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a number. */
public abstract class LongLongBooleanOp extends TernaryMapOperation<Long, AbstractLongStorage> {

    public LongLongBooleanOp(String name) {
        super(name);
    }

    protected abstract long doLongBoolean(long a, long b, boolean c, int ix, MapOperationProblemBuilder problemBuilder);

    @Override
    public Storage<Long> runTernaryMap(AbstractLongStorage storage, Object arg0, Object arg1, MapOperationProblemBuilder problemBuilder) {
        if (arg0 == null || arg1 == null) {
            return LongStorage.makeEmpty(storage.size());
        }

        long longArg;
        if (arg0 instanceof Long) {
            longArg = (Long) arg0;
        } else {
            throw new UnexpectedTypeException("a long.");
        }
        boolean booleanArg;
        if (arg1 instanceof Boolean) {
            booleanArg = (Boolean) arg1;
        } else {
            throw new UnexpectedTypeException("a boolean.");
        }

        Context context = Context.getCurrent();
        long[] out = new long[storage.size()];
        BitSet isMissing = new BitSet();

        for (int i = 0; i < storage.size(); i++) {
            if (!storage.isNa(i)) {
                long item = storage.getItem(i);
                out[i] = doLongBoolean(item, longArg, booleanArg, i, problemBuilder);
            } else {
                isMissing.set(i);
            }

            context.safepoint();
        }
        return new LongStorage(out, storage.size(), storage.getIsMissing());
    }
}
