package org.enso.os.environment;

import java.util.List;
import org.enso.os.environment.chdir.TestChangeDirectory;
import org.junit.runner.JUnitCore;
import org.junit.runner.notification.Failure;

public class TestRunner {
  public static void main(String[] args) {
    var junit = new JUnitCore();
    var result = junit.run(TestChangeDirectory.class);
    System.out.println("Test run finished.");
    System.out.println("Number of tests run: " + result.getRunCount());
    System.out.println("Number of tests failed: " + result.getFailureCount());
    System.out.println("Number of tests ignored: " + result.getIgnoreCount());
    System.out.println("Test run time: " + result.getRunTime() + "ms");
    System.out.println("Test run successful: " + result.wasSuccessful());
    if (!result.wasSuccessful()) {
      if (result.getFailureCount() > 0) {
        printFailures(result.getFailures());
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
}
