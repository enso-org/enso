package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a number. */
public abstract class DoubleLongBooleanOp extends TernaryMapOperation<Double, DoubleStorage> {

    public DoubleLongBooleanOp(String name) {
        super(name);
    }

    protected abstract double doLongBoolean(double a, long b, boolean c, int ix, MapOperationProblemBuilder problemBuilder);

    @Override
    public Storage<Double> runTernaryMap(DoubleStorage storage, Object arg0, Object arg1, MapOperationProblemBuilder problemBuilder) {
        if (arg0 == null || arg1 == null) {
            return DoubleStorage.makeEmpty(storage.size());
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
        for (int i = 0; i < storage.size(); i++) {
            if (!storage.isNa(i)) {
                out[i] = Double.doubleToRawLongBits(doLongBoolean(storage.getItem(i), longArg, booleanArg, i, problemBuilder));
            }

            context.safepoint();
        }
        return new DoubleStorage(out, storage.size(), storage.getIsMissing());
    }
}
