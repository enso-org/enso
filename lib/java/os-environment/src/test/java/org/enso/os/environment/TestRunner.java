package org.enso.os.environment;

import java.util.ArrayList;
import java.util.List;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class TestRunner {
  public static void main(String[] args) throws Throwable {
    var junit = new JUnitCore();
    var results = new ArrayList<Result>();
    for (var testClass : getAllTests()) {
      var result = junit.run(testClass);
      results.add(result);
    }
    printSummary(results);
  }

  private static void printSummary(List<Result> results) {
    var runTests = results.stream().mapToInt(Result::getRunCount).sum();
    var failedTests = results.stream().map(Result::getFailures).flatMap(List::stream).toList();
    var ignoredTests = results.stream().map(Result::getIgnoreCount).mapToInt(m -> m).sum();
    System.out.printf("Test run (of %s classes) finished.\n", results.size());
    System.out.printf("Number of tests: %s\n", runTests);
    System.out.printf("Number of tests failed: %s\n", failedTests.size());
    System.out.printf("Number of tests ignored: %s\n", ignoredTests);
    var success = failedTests.isEmpty();
    System.out.printf("Test run successful: %s", success);
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
}
