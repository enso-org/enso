package org.enso.interpreter;

import org.apache.commons.cli.*;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;

import java.io.File;
import java.io.IOException;

/** The main CLI entry point class. */
public class Main {
  private static final String RUN_OPTION = "run";
  private static final String HELP_OPTION = "help";

  /**
   * Builds the {@link Options} object representing the CLI syntax.
   *
   * @return an {@link Options} object representing the CLI syntax
   */
  private static Options buildOptions() {
    Option help = Option.builder("h").longOpt(HELP_OPTION).desc("Displays this message.").build();
    Option run =
        Option.builder()
            .hasArg(true)
            .numberOfArgs(1)
            .argName("file")
            .longOpt(RUN_OPTION)
            .desc("Runs a specified Enso file.")
            .build();

    Options options = new Options();
    options.addOption(help).addOption(run);
    return options;
  }

  /**
   * Prints the help message to the standard output.
   *
   * @param options object representing the CLI syntax
   */
  public static void printHelp(Options options) {
    new HelpFormatter().printHelp(Constants.LANGUAGE_ID, options);
  }

  /** Terminates the process with a failure exit code. */
  public static void exitFail() {
    System.exit(1);
  }

  /** Terminates the process with a success exit code. */
  public static void exitSuccess() {
    System.exit(0);
  }

  /**
   * Main entry point for the CLI program.
   *
   * @param args the command line arguments
   */
  public static void main(String[] args) throws IOException {
    Options options = buildOptions();
    CommandLineParser parser = new DefaultParser();
    CommandLine line;
    try {
      line = parser.parse(options, args);
    } catch (ParseException e) {
      printHelp(options);
      exitFail();
      return;
    }
    if (line.hasOption(HELP_OPTION)) {
      printHelp(options);
      exitSuccess();
      return;
    }
    if (!line.hasOption(RUN_OPTION)) {
      printHelp(options);
      exitFail();
      return;
    }

    File file = new File(line.getOptionValue(RUN_OPTION));

    Context context =
        Context.newBuilder(Constants.LANGUAGE_ID).out(System.out).in(System.in).build();
    Source source = Source.newBuilder(Constants.LANGUAGE_ID, file).build();
    context.eval(source);
  }
}
