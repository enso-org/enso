package org.enso.compiler.test.ircompare;

import static scala.jdk.javaapi.CollectionConverters.asScala;

import java.util.List;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.junit.Test;
import scala.Option;

public final class TestComparator {
  @Test
  public void compareSingleNodes() {
    var expectedLit = lit("foo", false);
    var actualLit = lit("foo", false);
    var comparator = buildComparator("compareSingleNodes");
    comparator.compare(expectedLit, actualLit);
  }

  @Test
  public void compareBlocks() {
    var expectedBlock = block(List.of(), lit("foo", false));
    var actualBlock = block(List.of(), lit("foo", false));
    var comparator = buildComparator("compareBLocks");
    comparator.compare(expectedBlock, actualBlock);
  }

  @Test
  public void compareBlocksWithSkip() {
    var expected = block(List.of(SkipIR.INSTANCE), lit("foo", false));
    var actual = block(List.of(lit("XXX", false)), lit("foo", false));
    var comparator = buildComparator("compareBlocksWithSkip");
    comparator.compare(expected, actual);
  }

  private static Name.Literal lit(String name, boolean isMethod) {
    return new Name.Literal(name, isMethod, null, Option.empty(), new MetadataStorage());
  }

  private static Expression.Block block(List<Expression> expressions, Expression returnExpr) {
    return new Expression.Block(
        asScala(expressions).toList(), returnExpr, null, false, new MetadataStorage());
  }

  private static IRComparator buildComparator(String name) {
    return IRComparator.builder().name(name).build();
  }
}
