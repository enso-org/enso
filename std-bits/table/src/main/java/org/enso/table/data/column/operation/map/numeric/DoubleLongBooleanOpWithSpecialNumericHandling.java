package org.enso.table.data.column.operation.map.numeric;

import org.enso.table.data.column.operation.map.TernaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.storage.numeric.DoubleStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.error.UnexpectedTypeException;
import org.graalvm.polyglot.Context;

import java.util.BitSet;

/** An operation expecting a numeric argument and returning a number. */
public abstract class DoubleLongBooleanOpWithSpecialNumericHandling extends TernaryMapOperation<Double, DoubleStorage> {

    public DoubleLongBooleanOpWithSpecialNumericHandling(String name) {
        super(name);
    }

    protected abstract double doLongBoolean(double a, long b, boolean c, int ix, MapOperationProblemBuilder problemBuilder);

    @Override
    public Storage<Double> runTernaryMap(DoubleStorage storage, Object arg0, Object arg1, MapOperationProblemBuilder problemBuilder) {
        if (arg0 == null || arg1 == null) {
            return DoubleStorage.makeEmpty(storage.size());
        }

        if (!(arg0 instanceof Long arg0AsLong)) {
            throw new UnexpectedTypeException("a long.");
        }
        if (!(arg1 instanceof Boolean arg1AsBoolean)) {
            throw new UnexpectedTypeException("a boolean.");
        }

        Context context = Context.getCurrent();
        long[] out = new long[storage.size()];
        BitSet isMissing = new BitSet();

        for (int i = 0; i < storage.size(); i++) {
            if (!storage.isNa(i)) {
                double item = storage.getItem(i);
                boolean special = Double.isNaN(item) || Double.isInfinite(item);
                if (!special) {
                    out[i] = Double.doubleToRawLongBits(doLongBoolean(item, arg0AsLong, arg1AsBoolean, i, problemBuilder));
                } else {
                    String msg = "Value is " + item;
                    problemBuilder.reportArithmeticError(msg, i);
                    isMissing.set(i);
                }
            } else {
                isMissing.set(i);
            }

            context.safepoint();
        }
        return new DoubleStorage(out, storage.size(), isMissing);
    }
}
