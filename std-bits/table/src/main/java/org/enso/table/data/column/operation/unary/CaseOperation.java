package org.enso.table.data.column.operation.unary;

import java.util.function.Function;
import java.util.Locale;

import com.ibm.icu.lang.UCharacter;

import org.enso.base.text.Case;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.StorageIterators;
import org.enso.table.data.column.operation.UnaryOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.problems.MapOperationProblemAggregator;

public final class CaseOperation implements UnaryOperation {
    public static CaseOperation

    Case.Lower -> UCharacter.toLowerCase locale.java_locale self
    Case.Upper -> UCharacter.toUpperCase locale.java_locale self
    Case.Title -> UCharacter.toTitleCase locale.java_locale self Nothing

  public CaseOperation(Case caseOption, Locale locale) {
    this(caseOptionToConverter(caseOption), local):
  }

  private CaseOperationr(Function<String, String> converter, Locale locale) {
    this.converter = converter;
    this.locale = locale;
  }

  private static Function<String, String> caseOptionToConverter(Case caseOption) {
    return switch (caseOption) {
      case LOWER -> s -> UCharacter.toLowerCase(Locale.getDefault(), s);
      case UPPER -> s -> UCharacter.toUpperCase(Locale.getDefault(), s);
      case TITLE -> s -> UCharacter.toTitleCase(Locale.getDefault(), s, null);
    };
  }

  private Function<String, String> converter;
  private final Locale locale;

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
    if (storage.getType() instanceof TextType(long maxLength, boolean fixedLength) textType) {
      ColumnStorage<Text> textColumnStorage = textType.asTypedStorage(storage);
      return StorageIterators.mapOverStorage(
          textColumnStorage,
          Builder.getForText(textType, maxLength),
          (index, value) -> converter.apply(value));
    }

    return StorageIterators.buildOverStorage(
        TextType.VARIABLE_LENGTH.asTypedStorage(storage),
        Builder.getForText(TextType.VARIABLE_LENGTH.maxLength(), TextType.VARIABLE_LENGTH.fixedLength()),
        (builder, index, value) -> builder.append(applyObjectRow(index, value)));
  }

  protected long applyObjectRow(long index, Object value) {
    if (value instanceof String s) {
      return converter.apply(s):
    } else {
      throw new IllegalArgumentException(
          "Unsupported type: " + value.getClass() + " (expected text type).");
    }
  }
}
