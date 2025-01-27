package org.enso.compiler.test.ircompare;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Name;
import org.enso.test.utils.IRDumperTestWrapper;

public final class IRComparator {
  private final String name;
  private final boolean compareMeta;
  private final IRDumperTestWrapper dumper = new IRDumperTestWrapper();

  private IRComparator(String name, boolean compareMeta) {
    this.name = name;
    this.compareMeta = compareMeta;
  }

  public static Builder builder() {
    return new Builder();
  }

  /**
   * Compares IRs and dumps the diff the {@code actualIR} IR does not match the {@code expectedIR}
   * one. IRs are compared recursively. {@code expectedIR} IR can have {@link SkipIR} nodes. When
   * the {@link IRComparator} encounters {@link SkipIR} node in the {@code expectedIR} IR, the
   * corresponding subtree in the {@code actualIR} IR is skipped.
   *
   * <p>Traverses the IRs in the BFS order.
   *
   * <p>If the comparison fails, the diff is dumped to the {@link IRDumperTestWrapper IGV}. And
   * {@link IRComparisonFailure} is thrown.
   *
   * @param expectedIR Can have {@link SkipIR} nodes.
   * @param actualIR
   */
  public void compare(IR expectedIR, IR actualIR) throws IRComparisonFailure {
    var nodesToProcess = new ArrayDeque<NodePair>();
    nodesToProcess.add(new NodePair(expectedIR, actualIR));
    while (!nodesToProcess.isEmpty()) {
      var nodePairToProcess = nodesToProcess.poll();
      var expected = nodePairToProcess.expected;
      var actual = nodePairToProcess.actual;
      if (expected instanceof SkipIR) {
        continue;
      }
      compareTwoNodes(expected, actual);
      var children = zipChildren(expected, actual);
      nodesToProcess.addAll(children);
    }
  }

  private void compareTwoNodes(IR expectedNode, IR actualNode) throws IRComparisonFailure {
    assert !(expectedNode instanceof SkipIR);
    var expectedClass = expectedNode.getClass().getName();
    var actualClass = actualNode.getClass().getName();
    if (!expectedClass.equals(actualClass)) {
      throw fail(
          "Expected node class " + expectedClass + " but got " + actualClass,
          expectedNode,
          actualNode);
    }
    var expectedChildrenCnt = expectedNode.children().size();
    var actualChildrenCnt = actualNode.children().size();
    if (expectedChildrenCnt != actualChildrenCnt) {
      throw fail(
          "Expected node to have " + expectedChildrenCnt + " children but got " + actualChildrenCnt,
          expectedNode,
          actualNode);
    }
    switch (expectedNode) {
      case Name lit -> compareTwoNodes(lit, (Name) actualNode);
      default -> {}
    }
  }

  private void compareTwoNodes(Name expectedName, Name actualName) {
    if (!expectedName.name().equals(actualName.name())) {
      throw fail(
          "Expected Name " + expectedName.name() + " but got " + actualName.name(),
          expectedName,
          actualName);
    }
    if (expectedName.isMethod() != actualName.isMethod()) {
      throw fail(
          "isMethod is different: expected: "
              + expectedName.isMethod()
              + " vs actual: "
              + actualName.isMethod(),
          expectedName,
          actualName);
    }
  }

  private static List<NodePair> zipChildren(IR expected, IR actual) {
    assert expected.children().size() == actual.children().size();
    var children = new ArrayList<NodePair>();
    for (var i = 0; i < expected.children().size(); i++) {
      var expectedChild = expected.children().apply(i);
      var actualChild = actual.children().apply(i);
      children.add(new NodePair(expectedChild, actualChild));
    }
    return children;
  }

  private IRComparisonFailure fail(String msg, IR expected, IR actual) {
    dumper.dump(expected, name, "expected");
    dumper.dump(actual, name, "actual");
    System.err.println("Dumped expected and actual IRs to IGV with name " + name);
    return new IRComparisonFailure(msg, expected, actual);
  }

  private record NodePair(IR expected, IR actual) {}

  public static final class Builder {
    private String name;
    private boolean compareMeta = false;

    public Builder name(String name) {
      this.name = name;
      return this;
    }

    public Builder compareMeta(boolean value) {
      this.compareMeta = value;
      return this;
    }

    public IRComparator build() {
      Objects.requireNonNull(name);
      if (compareMeta) {
        throw new IllegalArgumentException("Meta comparison is not supported yet");
      }
      return new IRComparator(name, compareMeta);
    }
  }
}
