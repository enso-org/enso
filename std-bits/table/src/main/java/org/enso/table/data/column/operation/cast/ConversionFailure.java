package org.enso.table.data.column.operation.cast;

import java.util.List;
import org.enso.base.polyglot.EnsoMeta;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.problems.Problem;
import org.graalvm.polyglot.Value;

public record ConversionFailure(
    ConversionFailureType errorType,
    StorageType<?> targetType,
    String relatedColumn,
    long affectedRowCount,
    List<?> examples)
    implements Problem {
  @Override
  public Value asEnsoValue() {
    var constructorName =
        switch (errorType()) {
          case NUMBER_OUT_OF_RANGE -> "Out_Of_Range";
          case TEXT_TOO_LONG -> "Text_Too_Long";
          case FAILED_CONVERSION -> "Error";
        };

    return EnsoMeta.makeInstance(
        "Standard.Table.Errors",
        "Conversion_Failure",
        constructorName,
        targetType().asEnsoValueType(),
        relatedColumn(),
        affectedRowCount(),
        examples());
  }
}
