package org.enso.table.data.column.operation.cast;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.Problem;

import java.util.List;

public record ConversionFailure(
    ConversionFailureType errorType,
    StorageType targetType,
    String relatedColumn,
    long affectedRowCount,
    List<?> examples
) implements Problem {
}
