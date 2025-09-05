package org.enso.table.data.column.operation.unary;

import java.util.Locale;
import java.util.function.Function;
import org.enso.base.Text_Utils;
import org.enso.base.text.Case;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public final class CaseOperation implements UnaryOperation {
  public CaseOperation(Case caseOption, Locale locale) {
    this(Text_Utils.caseOptionToConverter(caseOption, locale));
  }

  private CaseOperation(Function<String, String> converter) {
    this.converter = converter;
  }

  private Function<String, String> converter;

  @Override
  public String getName() {
    return "to_case";
  }

  @Override
  public boolean canApply(ColumnStorage<?> storage) {
    return storage.getType() instanceof TextType;
  }

  @Override
  public ColumnStorage<?> apply(
      ColumnStorage<?> storage, MapOperationProblemAggregator problemAggregator) {
    if (storage.getType() instanceof TextType textType) {
      ColumnStorage<String> textColumnStorage = textType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          textColumnStorage,
          Builder.getForText(textType, storage.getSize()),
          (index, value) -> converter.apply(value));
    }

    return StorageIterators.buildOverStorage(
        TextType.VARIABLE_LENGTH.asTypedStorage(storage),
        Builder.getForText(TextType.VARIABLE_LENGTH, TextType.VARIABLE_LENGTH.maxLength()),
        (builder, index, value) -> builder.append(applyObjectRow(index, value)));
  }

  protected String applyObjectRow(long index, Object value) {
    if (value instanceof String s) {
      return converter.apply(s);
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected text type).");
    }
  }
}
