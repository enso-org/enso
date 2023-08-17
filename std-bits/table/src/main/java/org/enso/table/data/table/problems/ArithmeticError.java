package org.enso.table.data.table.problems;

public class ArithmeticError extends ColumnAggregatedProblems {
    private final String message;

    public ArithmeticError(String locationName, String message, Integer row) {
        super(locationName, row);
        this.message = message;
    }

    @Override
    public boolean merge(ColumnAggregatedProblems another) {
        if (another instanceof ArithmeticError arithmeticError
                && this.getLocationName().equals(arithmeticError.getLocationName())
                && this.message.equals(arithmeticError.message)) {
            this.rows.addAll(another.rows);
            return true;
        }

        return false;
    }

    @Override
    public String getMessage() {
        return message + " (at rows " + makeTruncatedRowsString() + ").";
    }
}
