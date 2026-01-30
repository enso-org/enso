package org.enso.table.data.column.operation.cast;

import java.util.List;
import org.enso.table.problems.Problem;

public record ConversionFailure(
    ConversionFailureType errorType,
    char targetTypeChar,
    long targetTypeSize,
    String relatedColumn,
    long affectedRowCount,
    List<?> examples)
    implements Problem {}
