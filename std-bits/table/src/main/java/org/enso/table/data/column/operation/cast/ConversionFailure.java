package org.enso.table.data.column.operation.cast;

import java.util.List;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.Problem;

public record ConversionFailure(
    ConversionFailureType errorType,
    StorageType targetType,
    String relatedColumn,
    long affectedRowCount,
    List<?> examples)
    implements Problem {}
