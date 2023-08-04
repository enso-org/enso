package org.enso.table.data.index;

import java.util.*;
import java.util.stream.IntStream;
import java.util.stream.Stream;
import org.enso.base.text.TextFoldingStrategy;
import org.enso.table.aggregations.Aggregator;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.error.TooManyColumnsException;
import org.enso.table.problems.AggregatedProblems;
import org.enso.table.util.NameDeduplicator;
import org.graalvm.polyglot.Context;

public class CrossTabIndex {
  private static final int MAXIMUM_CROSS_TAB_COLUMN_COUNT = 10000;

  private Column[] xColumns;

  private Column[] yColumns;

  private MultiValueIndex<CombinedKey<UnorderedMultiValueKey, UnorderedMultiValueKey>> combinedIndex;

  private ObjectNumberer<UnorderedMultiValueKey> xKeyNumberer;

  private ObjectNumberer<UnorderedMultiValueKey> yKeyNumberer;

  private CombinedKey<?, ?>[][] grid;

  public CrossTabIndex(Column[] xColumns, Column[] yColumns, int tableSize) {
    this.xColumns = xColumns;
    this.yColumns = yColumns;

    // Create combined index
    combinedIndex =
        MultiValueIndex.makeCombinedIndex(
            xColumns, yColumns, tableSize, TextFoldingStrategy.unicodeNormalizedFold);

    // Fill numberings
    ObjectNumberer<UnorderedMultiValueKey> xKeyNumberer = new ObjectNumberer<>();
    ObjectNumberer<UnorderedMultiValueKey> yKeyNumberer = new ObjectNumberer<>();
    combinedIndex.keys().stream().map(ck -> ck.getKeyA()).forEach(k -> xKeyNumberer.add(k));
    combinedIndex.keys().stream().map(ck -> ck.getKeyB()).forEach(k -> yKeyNumberer.add(k));

    // Create grid of cells, mapping x and y key indices to combined keys.
    grid = new CombinedKey<?, ?>[numXKeys()][numYKeys()];

    // For each combined key, use the two subkeys to determine row+col
    // coordinates, and put the key at those coordinates.
    for (var combinedKey : combinedIndex.keys()) {
      int xCoordinate = getXCoordinate(combinedKey.getKeyA());
      int yCoordinate = getYCoordinate(combinedKey.getKeyB());

      // The pair (xCoordinate, yCoordinate) must be unique so this
      // check is not really necessary.
      if (grid[xCoordinate][yCoordinate] != null) {
        throw new IllegalStateException("Internal error: makeCrossTabTable coordinate conflict");
      }

      grid[xCoordinate][yCoordinate] = combinedKey;
    }
  }

  public List<Integer> get(UnorderedMultiValueKey xKey, UnorderedMultiValueKey yKey) {
    @SuppressWarnings("unchecked")
    List<Integer> result = combinedIndex.get((CombinedKey<UnorderedMultiValueKey, UnorderedMultiValueKey>) grid[getXCoordinate(xKey)][getYCoordinate(yKey)]);
    return result;
  }

  public Set<UnorderedMultiValueKey> getXKeys() {
    return xKeyNumberer.getObjects();
  }

  public Set<UnorderedMultiValueKey> getYKeys() {
    return yKeyNumberer.getObjects();
  }

  public int getXCoordinate(UnorderedMultiValueKey key) {
    return xKeyNumberer.getNumber(key);
  }

  public int getYCoordinate(UnorderedMultiValueKey key) {
    return yKeyNumberer.getNumber(key);
  }

  public int numXKeys() {
    return xKeyNumberer.size();
  }

  public int numYKeys() {
    return yKeyNumberer.size();
  }

  public Table makeCrossTabTable(Aggregator[] aggregates, String[] aggregateNames) {
    Context context = Context.getCurrent();
    NameDeduplicator outputTableNameDeduplicator = new NameDeduplicator();

    final int columnCount = yColumns.length + numXKeys() * aggregates.length;
    if (columnCount > MAXIMUM_CROSS_TAB_COLUMN_COUNT) {
      throw new TooManyColumnsException(
          "The cross_tab contained too many columns. Maximum allowed is "
              + MAXIMUM_CROSS_TAB_COLUMN_COUNT
              + " but was "
              + columnCount
              + ".",
          columnCount,
          MAXIMUM_CROSS_TAB_COLUMN_COUNT);
    }

    // Create the storage
    Builder[] storage = new Builder[columnCount];
    for (int i = 0; i < yColumns.length; i++) {
      storage[i] = Builder.getForType(yColumns[i].getStorage().getType(), numYKeys());
      context.safepoint();
    }

    for (int i = 0; i < numXKeys(); i++) {
      int offset = yColumns.length + i * aggregates.length;
      for (int j = 0; j < aggregates.length; j++) {
        storage[offset + j] = Builder.getForType(aggregates[j].getType(), numYKeys());
        context.safepoint();
      }
    }

    // Fill the columns.
    var emptyList = new ArrayList<Integer>();
    for (UnorderedMultiValueKey ySubKey : getYKeys()) {

      // Fill the y key columns.
      IntStream.range(0, yColumns.length).forEach(i -> storage[i].appendNoGrow(ySubKey.get(i)));

      int offset = yColumns.length;

      // Fill the aggregate columns.
      for (UnorderedMultiValueKey xSubKey : getXKeys()) {
        List<Integer> rowIds = get(xSubKey, ySubKey);
        if (rowIds == null) {
          rowIds = emptyList;
        }

        for (int i = 0; i < aggregates.length; i++) {
          storage[offset + i].appendNoGrow(aggregates[i].aggregate(rowIds));
        }

        offset += aggregates.length;
        context.safepoint();
      }
    }

    // Create Columns
    Column[] output = new Column[columnCount];
    for (int i = 0; i < yColumns.length; i++) {
      outputTableNameDeduplicator.markUsed(yColumns[i].getName());
      output[i] = new Column(yColumns[i].getName(), storage[i].seal());
      context.safepoint();
    }

    int offset = yColumns.length;
    for (UnorderedMultiValueKey xSubKey : getXKeys()) {
      // Use the nameColumn value as the new column name
      Object boxed = xSubKey.get(0);
      String name = boxed == null ? null : boxed.toString();
      // We want to fail hard on invalid colum names stemming from invalid input values and make
      // the user fix the data before cross_tab, to avoid data corruption.
      Column.ensureNameIsValid(name);

      for (int i = 0; i < aggregates.length; i++) {
        String effectiveName;
        if (aggregateNames[i].isEmpty()) {
          effectiveName = name;
        } else if (name.isEmpty()) {
          effectiveName = aggregateNames[i];
        } else {
          effectiveName = name + " " + aggregateNames[i];
        }

        // Check again to ensure that the appended aggregate name does not invalidate the name.
        // We do not check aggregateName itself before, because it _is_ allowed for it to be empty -
        // meaning just key names will be used and that is fine.
        Column.ensureNameIsValid(effectiveName);
        effectiveName = outputTableNameDeduplicator.makeUnique(effectiveName);

        output[offset + i] = new Column(effectiveName, storage[offset + i].seal());
        context.safepoint();
      }

      offset += aggregates.length;
    }

    // Merge Problems
    AggregatedProblems[] problems = new AggregatedProblems[aggregates.length + 2];
    problems[0] = combinedIndex.getProblems();
    problems[1] = AggregatedProblems.of(outputTableNameDeduplicator.getProblems());
    for (int i = 0; i < aggregates.length; i++) {
      problems[i + 2] = aggregates[i].getProblems();
      context.safepoint();
    }
    AggregatedProblems merged = AggregatedProblems.merge(problems);

    return new Table(output, merged);
  }
}
