package org.enso.interpreter.bench;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion.VersionFlag;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.enso.interpreter.bench.result.Result;
import org.enso.interpreter.bench.result.Results;
import org.openjdk.jmh.results.RunResult;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.CommandLineOptionException;
import org.openjdk.jmh.runner.options.CommandLineOptions;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import org.openjdk.jmh.runner.options.TimeValue;

/** Runner class for the benchmarks. Discovers, runs and reports benchmark results. */
public class BenchmarksRunner {
  private final File resultsFile;
  private static final String schemaFileName = "results_schema.json";

  public BenchmarksRunner() {
    this.resultsFile = new File("./bench-results.json");
  }

  public void run(String[] args) throws RunnerException {
    CommandLineOptions cmdOpts = null;
    try {
      cmdOpts = new CommandLineOptions(args);
    } catch (CommandLineOptionException e) {
      System.err.println("Error parsing command line args:");
      System.err.println("  " + e.getMessage());
      System.exit(1);
    }

    if (Boolean.getBoolean("bench.compileOnly")) {
      // Do not report results from `compileOnly` mode
      runCompileOnly(cmdOpts.getIncludes());
    } else {
      Runner jmhRunner = new Runner(cmdOpts);

      if (cmdOpts.shouldHelp()) {
        System.err.println("Enso benchmark runner: A modified JMH runner for Enso benchmarks.");
        try {
          cmdOpts.showHelp();
        } catch (IOException e) {
          throw new IllegalStateException("Unreachable", e);
        }
        System.exit(0);
      }

      if (cmdOpts.shouldList()) {
        jmhRunner.list();
        System.exit(0);
      }

      Collection<RunResult> results;
      results = jmhRunner.run();

      for (RunResult result : results) {
        try {
          reportResult(result);
        } catch (IOException e) {
          throw new IllegalStateException("Benchmark result report writing failed", e);
        }
      }
      try {
        validateResultsSchema();
      } catch (IOException e) {
        throw new IllegalStateException("Benchmark results schema validation failed", e);
      }
      System.out.println("Benchmark results reported into " + resultsFile.getAbsolutePath());
    }
  }

  /**
   * Results from compileOnly mode are not reported. Moreover, if some of the benchmarks in this
   * mode fails, the whole process immediately fails. This behavior is different to *normal*
   * benchmarks, where a single failure does not stop the whole process.
   */
  private static void runCompileOnly(List<String> includes) {
    if (includes.isEmpty()) {
      System.out.println("Running all benchmarks in compileOnly mode");
    } else {
      System.out.println("Running benchmarks " + includes + " in compileOnly mode");
    }
    var optsBuilder =
        new OptionsBuilder()
            .measurementTime(TimeValue.seconds(1))
            .measurementIterations(1)
            .warmupIterations(0)
            .shouldFailOnError(true)
            .forks(0);
    includes.forEach(optsBuilder::include);
    var opts = optsBuilder.build();
    var runner = new Runner(opts);
    try {
      runner.run();
      System.out.println(
          "benchmarks run successfully in compileOnly mode. Results are not reported.");
    } catch (RunnerException e) {
      System.err.println("Benchmark run failed: " + e.getMessage());
      e.printStackTrace(System.err);
      System.exit(1);
    }
  }

  private void reportResult(RunResult result) throws IOException {
    if (!resultsFile.exists()) {
      resultsFile.createNewFile();
      var results = new Results(schemaFileName, new ArrayList<>());
      ObjectMapper mapper = new ObjectMapper();
      mapper.writeValue(resultsFile, results);
    }

    ObjectMapper mapper = new ObjectMapper();
    var results = mapper.readValue(resultsFile, Results.class);
    var res = Result.fromJMHResult(result);
    results.results().add(res);
    JsonFactory factory = new JsonFactory();
    var resultsGenerator = factory.createGenerator(resultsFile, JsonEncoding.UTF8);
    resultsGenerator.useDefaultPrettyPrinter();
    mapper.writeValue(resultsGenerator, results);
  }

  private void validateResultsSchema() throws IOException {
    var schemaFactory = JsonSchemaFactory.getInstance(VersionFlag.V7);
    var mapper = new ObjectMapper();
    var resultsJson = mapper.readTree(resultsFile);
    try (var schemaStream = getClass().getClassLoader().getResourceAsStream(schemaFileName)) {
      var schema = schemaFactory.getSchema(schemaStream);
      var validationMsgs = schema.validate(resultsJson);
      for (var validationMsg : validationMsgs) {
        if (!validationMsg.isValid()) {
          System.err.println("Schema validation failed: " + validationMsg);
          System.exit(1);
        }
      }
    }
  }
}
