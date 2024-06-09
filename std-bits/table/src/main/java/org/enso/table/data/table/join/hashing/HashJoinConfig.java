package org.enso.table.data.table.join.hashing;

import java.util.List;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.join.JoinStrategy;
import org.enso.table.data.table.join.conditions.Equals;
import org.enso.table.data.table.join.conditions.EqualsIgnoreCase;
import org.enso.table.data.table.join.conditions.HashableCondition;

public class HashJoinConfig {

  private final Column[] leftEquals;
  private final Column[] rightEquals;
  private final List<TextFoldingStrategy> textFoldingStrategies;

  public HashJoinConfig(List<HashableCondition> conditions) {
    JoinStrategy.ensureConditionsNotEmpty(conditions);
    List<HashEqualityCondition> equalConditions =
        conditions.stream().map(HashJoinConfig::makeHashEqualityCondition).toList();

    this.leftEquals =
        equalConditions.stream().map(HashEqualityCondition::left).toArray(Column[]::new);
    this.rightEquals =
        equalConditions.stream().map(HashEqualityCondition::right).toArray(Column[]::new);
    this.textFoldingStrategies =
        equalConditions.stream().map(HashEqualityCondition::textFoldingStrategy).toList();
  }

  public HashJoinConfig(
      Column[] leftEquals, Column[] rightEquals, List<TextFoldingStrategy> textFoldingStrategies) {
    this.leftEquals = leftEquals;
    this.rightEquals = rightEquals;
    this.textFoldingStrategies = textFoldingStrategies;
  }

  public Column[] getLeftEquals() {
    return leftEquals;
  }

  public Column[] getRightEquals() {
    return rightEquals;
  }

  public int getLeftNumRows() {
    return leftEquals[0].getSize();
  }

  public int getRightNumRows() {
    return rightEquals[0].getSize();
  }

  public List<TextFoldingStrategy> getTextFoldingStrategies() {
    return textFoldingStrategies;
  }

  private static HashEqualityCondition makeHashEqualityCondition(HashableCondition eq) {
    switch (eq) {
      case Equals e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.unicodeNormalizedFold);
      }
      case EqualsIgnoreCase e -> {
        return new HashEqualityCondition(
            e.left(), e.right(), TextFoldingStrategy.caseInsensitiveFold(e.locale()));
      }
    }
  }

  private record HashEqualityCondition(
      Column left, Column right, TextFoldingStrategy textFoldingStrategy) {}
}
