package org.enso.os.environment;

import java.util.ArrayList;
import java.util.List;
import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

public class TestRunner {
  public static void main(String[] args) {
    var junit = new JUnitCore();
    var results = new ArrayList<Result>();
    for (var testClass : getAllTests()) {
      var result = junit.run(testClass);
      results.add(result);
    }
    printSummary(results);
  }

  private static void printSummary(List<Result> results) {
    var runTests = results.stream().map(Result::getRunCount).mapToInt(m -> m).sum();
    var failedTests = results.stream().filter(r -> !r.wasSuccessful()).toList();
    var ignoredTests = results.stream().map(Result::getIgnoreCount).mapToInt(m -> m).sum();
    System.out.println("Test run finished.");
    System.out.println("Number of test classes: " + results.size());
    System.out.println("Number of tests failed: " + failedTests.size());
    System.out.println("Number of tests ignored: " + ignoredTests);
    System.out.println("Number of successful tests: " + runTests);
    var success = failedTests.isEmpty();
    System.out.println("Test run successful: " + success);
    if (!success) {
      for (var failedTest : failedTests) {
        printFailures(failedTest.getFailures());
      }
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
