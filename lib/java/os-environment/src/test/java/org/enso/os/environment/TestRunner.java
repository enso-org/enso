package org.enso.os.environment;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import org.junit.runner.Description;
import org.junit.runner.JUnitCore;
import org.junit.runner.Request;
import org.junit.runner.Result;
import org.junit.runner.manipulation.Filter;
import org.junit.runner.notification.Failure;

public class TestRunner {
  public static void main(String[] args) throws Throwable {
    var junit = new JUnitCore();
    var req = Request.classes(getAllTests().toArray(Class[]::new));
    if (args.length > 0) {
      req = req.filterWith(new FilterByMethodName(args));
    }
    var result = junit.run(req);
    printSummary(Collections.singletonList(result));
  }

  private static void printSummary(List<Result> results) {
    var runTests = results.stream().mapToInt(Result::getRunCount).sum();
    var failedTests = results.stream().map(Result::getFailures).flatMap(List::stream).toList();
    var ignoredTests = results.stream().map(Result::getIgnoreCount).mapToInt(m -> m).sum();
    System.out.printf("Test run finished.\n");
    System.out.printf("Number of tests: %s\n", runTests);
    System.out.printf("Number of tests failed: %s\n", failedTests.size());
    System.out.printf("Number of tests ignored: %s\n", ignoredTests);
    var success = failedTests.isEmpty();
    System.out.printf("Test run successful: %s\n", success);
    if (!success) {
      printFailures(failedTests);
      System.exit(1);
    }
  }

  private static void printFailures(List<Failure> failures) {
    for (var failure : failures) {
      System.out.println("Description:");
      System.out.println(failure.getDescription());
      System.out.println("Message:");
      System.out.println(failure.getMessage());
      System.out.println("Stack:");
      failure.getException().printStackTrace(System.out);
    }
    System.out.println("Re-run as:");
    for (var failure : failures) {
      System.out.println(
          ProcessHandle.current().info().command().get()
              + " "
              + failure.getDescription().getMethodName());
    }
  }

  private static List<Class<?>> getAllTests() {
    var testClasses = new ArrayList<Class<?>>();
    for (var testClassName : ListOfTests.TEST_CLASSES) {
      try {
        testClasses.add(Class.forName(testClassName));
      } catch (ClassNotFoundException e) {
        throw new IllegalStateException(e);
      }
    }
    return testClasses;
  }

  private static final class FilterByMethodName extends Filter {
    private final String[] args;

    FilterByMethodName(String[] args) {
      this.args = args;
    }

    @Override
    public boolean shouldRun(Description description) {
      if (description.isSuite()) {
        return true;
      }
      var run = Arrays.asList(args).contains(description.getMethodName());
      if (!run) {
        System.err.println("skipping " + description.getMethodName());
      }
      return run;
    }

    @Override
    public String describe() {
      return "Filter" + Arrays.toString(args);
    }

    @Override
    public String toString() {
      return describe();
    }
  }
}
